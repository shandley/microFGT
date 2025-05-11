#' @import methods
NULL

#' FGTExperiment Class
#'
#' @name FGTExperiment-class
#' @rdname FGTExperiment-class
#' @slot experimentType Character string indicating the experiment type
#' @slot fgtMetadata SimpleList containing additional metadata
#'
#' @importClassesFrom TreeSummarizedExperiment TreeSummarizedExperiment
#' @importFrom S4Vectors SimpleList
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @exportClass FGTExperiment
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

#' Get the experiment type
#'
#' @param x FGTExperiment object
#'
#' @return Character string indicating the experiment type
#' @export
#' @rdname FGTExperiment-accessors
setGeneric("experimentType", function(x) standardGeneric("experimentType"))

#' Set the experiment type
#'
#' @param x FGTExperiment object
#' @param value New experiment type value
#'
#' @return Updated FGTExperiment object
#' @export
#' @rdname FGTExperiment-accessors
setGeneric("experimentType<-", function(x, value) standardGeneric("experimentType<-"))

#' Get the FGT metadata
#'
#' @param x FGTExperiment object
#'
#' @return SimpleList containing the metadata
#' @export
#' @rdname FGTExperiment-accessors
setGeneric("fgtMetadata", function(x) standardGeneric("fgtMetadata"))

#' Set the FGT metadata
#'
#' @param x FGTExperiment object
#' @param value SimpleList containing the new metadata
#'
#' @return Updated FGTExperiment object
#' @export
#' @rdname FGTExperiment-accessors
setGeneric("fgtMetadata<-", function(x, value) standardGeneric("fgtMetadata<-"))

#' @rdname FGTExperiment-accessors
#' @export
setMethod("experimentType", "FGTExperiment", function(x) {
  x@experimentType
})

#' @rdname FGTExperiment-accessors
#' @export
setMethod("experimentType<-", "FGTExperiment", function(x, value) {
  valid_types <- c("amplicon", "metagenomic", "integrated")
  if (!value %in% valid_types) {
    stop("experimentType must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  x@experimentType <- value
  return(x)
})

#' @rdname FGTExperiment-accessors
#' @export
setMethod("fgtMetadata", "FGTExperiment", function(x) {
  x@fgtMetadata
})

#' @rdname FGTExperiment-accessors
#' @export
setMethod("fgtMetadata<-", "FGTExperiment", function(x, value) {
  if (!methods::is(value, "SimpleList")) {
    stop("fgtMetadata must be a SimpleList")
  }
  
  x@fgtMetadata <- value
  return(x)
})