#' FGTExperiment Class
#'
#' The FGTExperiment class extends TreeSummarizedExperiment to provide specialized
#' functionality for female genital tract (FGT) microbiome data analysis. It adds
#' slots for experiment type and custom metadata.
#'
#' @slot experimentType Character string indicating the type of experiment
#'   ("amplicon", "metagenomic", or "integrated").
#' @slot fgtMetadata SimpleList containing experiment-specific metadata.
#'
#' @name FGTExperiment-class
#' @rdname FGTExperiment-class
#' @exportClass FGTExperiment
#'
#' @examples
#' \dontrun{
#' # Create a simple FGTExperiment
#' counts <- matrix(sample(1:100, 60), nrow = 10, ncol = 6)
#' rownames(counts) <- paste0("Feature", 1:10)
#' colnames(counts) <- paste0("Sample", 1:6)
#'
#' # Create a basic FGTExperiment
#' fgt_exp <- FGTExperiment(assays = list(counts = counts))
#' }
#' @importClassesFrom TreeSummarizedExperiment TreeSummarizedExperiment
#' @importFrom S4Vectors SimpleList
#' @importFrom methods setClass setMethod setGeneric setValidity show new is
#' @importFrom SummarizedExperiment SummarizedExperiment assayNames assayNames<-
setClass("FGTExperiment",
         contains = "TreeSummarizedExperiment",
         slots = list(
           experimentType = "character",
           fgtMetadata = "SimpleList"
         ),
         prototype = list(
           experimentType = "amplicon",
           fgtMetadata = S4Vectors::SimpleList()
         ))

#' Validity method for FGTExperiment objects
#'
#' @param object FGTExperiment object to validate
#'
#' @return TRUE if the object is valid, otherwise an error message
setValidity("FGTExperiment", function(object) {
  errors <- character()
  
  # Validate experimentType
  valid_types <- c("amplicon", "metagenomic", "integrated")
  if (length(object@experimentType) != 1 || 
      !object@experimentType %in% valid_types) {
    errors <- c(errors, paste("experimentType must be one of:",
                              paste(valid_types, collapse = ", ")))
  }
  
  # Validate fgtMetadata
  if (!is(object@fgtMetadata, "SimpleList")) {
    errors <- c(errors, "fgtMetadata must be a SimpleList object")
  }
  
  # Return TRUE if no errors, otherwise return error messages
  if (length(errors) == 0) TRUE else errors
})

#' Create FGTExperiment Object
#'
#' Constructor function for creating FGTExperiment objects.
#' This is the primary recommended constructor.
#'
#' @param assays List of matrices or similar objects containing count or abundance data
#' @param rowData DataFrame of feature metadata (e.g., taxonomic classifications)
#' @param colData DataFrame of sample metadata
#' @param rowTree phylo object or NULL for feature tree (e.g., phylogenetic tree)
#' @param experimentType Type of experiment ("amplicon", "metagenomic", "integrated")
#' @param fgtMetadata Additional metadata specific to FGT analysis
#' @param ... Additional arguments passed to TreeSummarizedExperiment constructor
#'
#' @return An FGTExperiment object.
#' @export
#'
#' @examples
#' # Create a simple FGTExperiment
#' counts <- matrix(sample(1:100, 60), nrow = 10, ncol = 6)
#' rownames(counts) <- paste0("Feature", 1:10)
#' colnames(counts) <- paste0("Sample", 1:6)
#'
#' # Basic usage
#' fgt_exp <- FGTExperiment(assays = list(counts = counts))
#'
#' # With experiment type and sample metadata
#' sample_data <- data.frame(
#'   group = rep(c("A", "B"), each = 3),
#'   row.names = colnames(counts)
#' )
#' fgt_exp2 <- FGTExperiment(
#'   assays = list(counts = counts),
#'   colData = sample_data,
#'   experimentType = "metagenomic"
#' )
FGTExperiment <- function(assays, rowData = NULL, colData = NULL, rowTree = NULL,
                        experimentType = "amplicon", fgtMetadata = S4Vectors::SimpleList(), ...) {
  
  # Input validation
  experimentType <- validate_param(experimentType, 
                                   c("amplicon", "metagenomic", "integrated"), 
                                   "experimentType")
  
  # Check that assays is a list with at least one element
  if (!is.list(assays) || length(assays) == 0) {
    stop(format_error("'assays' must be a non-empty list", "FGTExperiment"))
  }
  
  # Verify the first assay (e.g., counts)
  first_assay <- assays[[1]]
  if (!is.matrix(first_assay) && !is(first_assay, "Matrix")) {
    stop(format_error("The first element of 'assays' must be a matrix or Matrix object", 
                      "FGTExperiment"))
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
  
  # Validate fgtMetadata
  if (!is(fgtMetadata, "SimpleList")) {
    stop(format_error("fgtMetadata must be a SimpleList", "FGTExperiment"))
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
                    fgtMetadata = fgtMetadata)
  
  # 4. Ensure assays are properly named
  if (length(assayNames(obj)) > 0 && assayNames(obj)[1] == "") {
    assayNames(obj)[1] <- "counts"
  }
  
  return(obj)
}

#' Show method for FGTExperiment objects
#'
#' @param object FGTExperiment object to display
#'
#' @return None, prints to console
#'
#' @importFrom methods show
#' @export
setMethod("show", "FGTExperiment", function(object) {
  callNextMethod()
  cat("experimentType:", object@experimentType, "\n")
  fgt_metadata_length <- length(object@fgtMetadata)
  
  if (fgt_metadata_length > 0) {
    cat("fgtMetadata elements:", paste(names(object@fgtMetadata), collapse = ", "), "\n")
  } else {
    cat("fgtMetadata elements: none \n")
  }
})

#' Get or set the experiment type
#'
#' @param x FGTExperiment object
#' @param value Character string for replacement
#'
#' @return For getter, a character string; for setter, updated FGTExperiment
#'
#' @export
#' @rdname experimentType
#' @aliases experimentType experimentType<-
setGeneric("experimentType", function(x) standardGeneric("experimentType"))

#' @rdname experimentType
setMethod("experimentType", "FGTExperiment", function(x) {
  x@experimentType
})

#' @rdname experimentType
#' @param x FGTExperiment object
#' @param value New experiment type
setGeneric("experimentType<-", function(x, value) standardGeneric("experimentType<-"))

#' @rdname experimentType
setMethod("experimentType<-", "FGTExperiment", function(x, value) {
  value <- validate_param(value, 
                          c("amplicon", "metagenomic", "integrated"), 
                          "experimentType")
  x@experimentType <- value
  return(x)
})

#' Get or set FGT metadata
#'
#' @param x FGTExperiment object
#' @param value SimpleList for replacement
#'
#' @return For getter, a SimpleList; for setter, updated FGTExperiment
#'
#' @export
#' @rdname fgtMetadata
#' @aliases fgtMetadata fgtMetadata<-
setGeneric("fgtMetadata", function(x) standardGeneric("fgtMetadata"))

#' @rdname fgtMetadata
setMethod("fgtMetadata", "FGTExperiment", function(x) {
  x@fgtMetadata
})

#' @rdname fgtMetadata
#' @param x FGTExperiment object
#' @param value New FGT metadata
setGeneric("fgtMetadata<-", function(x, value) standardGeneric("fgtMetadata<-"))

#' @rdname fgtMetadata
setMethod("fgtMetadata<-", "FGTExperiment", function(x, value) {
  if (!is(value, "SimpleList")) {
    stop(format_error("fgtMetadata must be a SimpleList", "fgtMetadata<-"))
  }
  x@fgtMetadata <- value
  return(x)
})

#' Helper function to check if FGTExperiment class exists
#'
#' @return Logical indicating if the class is available
#' @keywords internal
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