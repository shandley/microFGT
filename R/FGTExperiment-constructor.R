#' FGTExperiment Constructor
#'
#' Create a new FGTExperiment object from count data.
#'
#' @param assays List of matrices or similar objects containing count data
#' @param rowData DataFrame of feature metadata
#' @param colData DataFrame of sample metadata
#' @param rowTree Phylogenetic tree of type phylo
#' @param experimentType Type of experiment ("amplicon", "metagenomic", "integrated")
#' @param fgtMetadata Additional metadata as a SimpleList
#' @param ... Additional arguments passed to TreeSummarizedExperiment
#'
#' @return A new FGTExperiment object
#' @export
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom S4Vectors SimpleList DataFrame
#' @rdname FGTExperiment-class
FGTExperiment <- function(assays, rowData = NULL, colData = NULL, rowTree = NULL,
                        experimentType = "amplicon", fgtMetadata = S4Vectors::SimpleList(), ...) {
  # Input validation
  if (!is.list(assays) || length(assays) == 0) {
    stop("'assays' must be a non-empty list")
  }

  # Verify the first assay (e.g., counts)
  first_assay <- assays[[1]]
  if (!is.matrix(first_assay) && !methods::is(first_assay, "Matrix")) {
    stop("The first element of 'assays' must be a matrix or Matrix object")
  }

  # Ensure row and column names are present
  if (is.null(rownames(first_assay))) {
    rownames(first_assay) <- paste0("Feature", seq_len(nrow(first_assay)))
    assays[[1]] <- first_assay
  }

  if (is.null(colnames(first_assay))) {
    colnames(first_assay) <- paste0("Sample", seq_len(ncol(first_assay)))
    assays[[1]] <- first_assay
  }

  # Prepare rowData if needed
  if (is.null(rowData)) {
    rowData <- S4Vectors::DataFrame(row.names = rownames(first_assay))
  } else if (!is.data.frame(rowData) && !methods::is(rowData, "DataFrame")) {
    rowData <- S4Vectors::DataFrame(rowData, row.names = rownames(first_assay))
  }

  # Prepare colData if needed
  if (is.null(colData)) {
    colData <- S4Vectors::DataFrame(row.names = colnames(first_assay))
  } else if (!is.data.frame(colData) && !methods::is(colData, "DataFrame")) {
    colData <- S4Vectors::DataFrame(colData, row.names = colnames(first_assay))
  }

  # Use two-step approach to bypass constructor issues
  # 1. Create SummarizedExperiment
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = assays,
    rowData = rowData,
    colData = colData,
    ...
  )

  # 2. Convert to TreeSummarizedExperiment
  tse <- tryCatch({
    TreeSummarizedExperiment::TreeSummarizedExperiment(se, rowTree = rowTree)
  }, error = function(e) {
    warning("Error creating TreeSummarizedExperiment: ", conditionMessage(e),
            ". Using SummarizedExperiment instead.")
    se
  })

  # 3. Create FGTExperiment object directly
  obj <- methods::new("FGTExperiment",
                     tse,
                     experimentType = experimentType,
                     fgtMetadata = fgtMetadata)

  return(obj)
}