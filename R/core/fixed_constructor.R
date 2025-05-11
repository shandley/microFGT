#' Fixed Constructor for FGTExperiment objects
#'
#' Creates an FGTExperiment object, which extends TreeSummarizedExperiment with
#' additional slots for FGT microbiome-specific data. This version adds additional
#' error checking and handling to fix the "invalid rownames length" error.
#'
#' @param assays List of matrices or similar objects containing count or abundance data
#' @param rowData DataFrame of feature metadata (e.g., taxonomic classifications)
#' @param colData DataFrame of sample metadata
#' @param rowTree phylo object or NULL for feature tree (e.g., phylogenetic tree)
#' @param experimentType Type of experiment ("amplicon", "metagenomic", "integrated")
#' @param ... Additional arguments passed to TreeSummarizedExperiment constructor
#'
#' @return An FGTExperiment object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a simple FGTExperiment
#' counts <- matrix(sample(1:100, 60), nrow = 10, ncol = 6)
#' rownames(counts) <- paste0("Feature", 1:10)
#' colnames(counts) <- paste0("Sample", 1:6)
#' 
#' # Create simple FGTExperiment
#' fgt_exp <- create_FGTExperiment(
#'   assays = list(counts = counts)
#' )
#' }
create_FGTExperiment <- function(assays, rowData = NULL, colData = NULL, rowTree = NULL,
                               experimentType = "amplicon", ...) {
  
  # Input validation
  if (!experimentType %in% c("amplicon", "metagenomic", "integrated")) {
    stop("experimentType must be one of: 'amplicon', 'metagenomic', 'integrated'")
  }
  
  # Check that assays is a list with at least one element
  if (!is.list(assays) || length(assays) == 0) {
    stop("'assays' must be a non-empty list")
  }
  
  # Verify the first assay (e.g., counts)
  first_assay <- assays[[1]]
  if (!is.matrix(first_assay) && !is(first_assay, "Matrix")) {
    stop("The first element of 'assays' must be a matrix or Matrix object")
  }
  
  # Ensure row and column names are properly set
  if (is.null(rownames(first_assay))) {
    rownames(first_assay) <- paste0("Feature", seq_len(nrow(first_assay)))
    warning("Row names were not provided in the assay matrix. Generated default names.")
    # Update the assay in the list
    assays[[1]] <- first_assay
  }
  
  if (is.null(colnames(first_assay))) {
    colnames(first_assay) <- paste0("Sample", seq_len(ncol(first_assay)))
    warning("Column names were not provided in the assay matrix. Generated default names.")
    # Update the assay in the list
    assays[[1]] <- first_assay
  }
  
  # Prepare rowData if needed
  if (is.null(rowData)) {
    rowData <- data.frame(row.names = rownames(first_assay))
  } else if (!is.data.frame(rowData) && !is(rowData, "DataFrame")) {
    rowData <- as.data.frame(rowData)
  }
  
  # Prepare colData if needed
  if (is.null(colData)) {
    colData <- data.frame(row.names = colnames(first_assay))
  } else if (!is.data.frame(colData) && !is(colData, "DataFrame")) {
    colData <- as.data.frame(colData)
  }
  
  # Ensure row names match between assay and metadata
  if (!is.null(rowData) && !is.null(rownames(rowData))) {
    if (length(rownames(rowData)) != nrow(first_assay)) {
      warning("rowData has different number of rows than assay. Using assay row names.")
      rowData <- data.frame(row.names = rownames(first_assay))
    }
  }
  
  if (!is.null(colData) && !is.null(rownames(colData))) {
    if (length(rownames(colData)) != ncol(first_assay)) {
      warning("colData has different number of rows than assay has columns. Using assay column names.")
      colData <- data.frame(row.names = colnames(first_assay))
    }
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
    message("Error creating TreeSummarizedExperiment: ", conditionMessage(e))
    # Return the SummarizedExperiment if TSE creation fails
    se
  })
  
  # 3. Create FGTExperiment object directly
  obj <- methods::new("FGTExperiment", 
                    tse,
                    experimentType = experimentType,
                    fgtMetadata = S4Vectors::SimpleList())
  
  # 4. Ensure assays are properly named
  if (length(assayNames(obj)) > 0 && assayNames(obj)[1] == "") {
    assayNames(obj)[1] <- "counts"
  }
  
  return(obj)
}

#' Helper function to check if FGTExperiment class exists
#'
#' @return Logical indicating if the class is available
is_fgt_available <- function() {
  if (!requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
    return(FALSE)
  }
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    return(FALSE)
  }
  
  # Check if FGTExperiment class is defined
  methods::existsClass("FGTExperiment")
}