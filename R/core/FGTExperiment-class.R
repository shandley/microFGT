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

#' Create FGTExperiment Object
#'
#' Constructor function for creating FGTExperiment objects.
#'
#' @param ... Arguments passed to TreeSummarizedExperiment constructor.
#' @param experimentType Type of experiment ("amplicon", "metagenomic", "integrated").
#' @param fgtMetadata Additional metadata specific to FGT analysis.
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
#' # With experiment type
#' fgt_exp2 <- FGTExperiment(
#'   assays = list(counts = counts),
#'   experimentType = "metagenomic"
#' )
FGTExperiment <- function(..., 
                        experimentType = "amplicon",
                        fgtMetadata = S4Vectors::SimpleList()) {
  
  # Validate experimentType
  if (!experimentType %in% c("amplicon", "metagenomic", "integrated")) {
    stop("experimentType must be one of: 'amplicon', 'metagenomic', 'integrated'")
  }
  
  # Create a TreeSummarizedExperiment
  tse <- TreeSummarizedExperiment::TreeSummarizedExperiment(...)
  
  # Create an FGTExperiment
  fgt <- new("FGTExperiment",
           tse,
           experimentType = experimentType,
           fgtMetadata = fgtMetadata)
  
  # Ensure first assay has a name
  if (length(assayNames(fgt)) > 0 && assayNames(fgt)[1] == "") {
    assayNames(fgt)[1] <- "counts"
  }
  
  return(fgt)
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
  if (!value %in% c("amplicon", "metagenomic", "integrated")) {
    stop("experimentType must be one of: 'amplicon', 'metagenomic', 'integrated'")
  }
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
    stop("fgtMetadata must be a SimpleList")
  }
  x@fgtMetadata <- value
  return(x)
})