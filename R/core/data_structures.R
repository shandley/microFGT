#' @title FGTExperiment Class
#' @description S4 class extending TreeSummarizedExperiment for FGT microbiome data
#'
#' @importClassesFrom TreeSummarizedExperiment TreeSummarizedExperiment
#' @importFrom S4Vectors SimpleList
#'
#' @export
setClass("FGTExperiment",
         contains = "TreeSummarizedExperiment",
         slots = list(
           experimentType = "character",
           fgtMetadata = "SimpleList"
         ),
         prototype = list(
           experimentType = "amplicon",
           fgtMetadata = S4Vectors::SimpleList()
         )
)

#' Constructor for FGTExperiment objects
#'
#' Creates an FGTExperiment object, which extends TreeSummarizedExperiment with
#' additional slots for FGT microbiome-specific data.
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
#' # Create a simple FGTExperiment
#' counts <- matrix(sample(1:100, 60), nrow = 10, ncol = 6)
#' rownames(counts) <- paste0("Feature", 1:10)
#' colnames(counts) <- paste0("Sample", 1:6)
#' 
#' # Sample metadata
#' sample_data <- data.frame(
#'   group = rep(c("A", "B"), each = 3),
#'   row.names = colnames(counts)
#' )
#' 
#' # Feature metadata
#' feature_data <- data.frame(
#'   Kingdom = rep("Bacteria", 10),
#'   Phylum = sample(c("Firmicutes", "Bacteroidetes"), 10, replace = TRUE),
#'   row.names = rownames(counts)
#' )
#' 
#' fgt_exp <- FGTExperiment(
#'   assays = list(counts = counts),
#'   rowData = feature_data,
#'   colData = sample_data
#' )
FGTExperiment <- function(assays, rowData = NULL, colData = NULL, rowTree = NULL,
                         experimentType = "amplicon", ...) {
  
  # Input validation
  if (!experimentType %in% c("amplicon", "metagenomic", "integrated")) {
    stop("experimentType must be one of: 'amplicon', 'metagenomic', 'integrated'")
  }
  
  # Create TreeSummarizedExperiment
  tse <- TreeSummarizedExperiment::TreeSummarizedExperiment(
    assays = assays,
    rowData = rowData,
    colData = colData,
    rowTree = rowTree,
    ...
  )
  
  # Create FGTExperiment object
  obj <- new("FGTExperiment", 
            tse,
            experimentType = experimentType,
            fgtMetadata = S4Vectors::SimpleList())
  
  return(obj)
}

#' Get experiment type
#'
#' @param x An FGTExperiment object
#'
#' @return A character string indicating the experiment type
#' @export
setGeneric("experimentType", function(x) standardGeneric("experimentType"))

#' @rdname experimentType
#' @export
setMethod("experimentType", "FGTExperiment", function(x) {
  return(x@experimentType)
})

#' Set experiment type
#'
#' @param x An FGTExperiment object
#' @param value A character string ("amplicon", "metagenomic", or "integrated")
#'
#' @return An updated FGTExperiment object
#' @export
setGeneric("experimentType<-", function(x, value) standardGeneric("experimentType<-"))

#' @rdname experimentType
#' @export
setMethod("experimentType<-", "FGTExperiment", function(x, value) {
  if (!value %in% c("amplicon", "metagenomic", "integrated")) {
    stop("experimentType must be one of: 'amplicon', 'metagenomic', 'integrated'")
  }
  x@experimentType <- value
  return(x)
})

#' Get FGT metadata
#'
#' @param x An FGTExperiment object
#'
#' @return A SimpleList containing FGT-specific metadata
#' @export
setGeneric("fgtMetadata", function(x) standardGeneric("fgtMetadata"))

#' @rdname fgtMetadata
#' @export
setMethod("fgtMetadata", "FGTExperiment", function(x) {
  return(x@fgtMetadata)
})

#' Set FGT metadata
#'
#' @param x An FGTExperiment object
#' @param value A SimpleList containing FGT-specific metadata
#'
#' @return An updated FGTExperiment object
#' @export
setGeneric("fgtMetadata<-", function(x, value) standardGeneric("fgtMetadata<-"))

#' @rdname fgtMetadata
#' @export
setMethod("fgtMetadata<-", "FGTExperiment", function(x, value) {
  if (!is(value, "SimpleList")) {
    stop("fgtMetadata must be a SimpleList")
  }
  x@fgtMetadata <- value
  return(x)
})

#' Show method for FGTExperiment objects
#'
#' @param object An FGTExperiment object
#'
#' @return NULL (invisibly)
#' @export
setMethod("show", "FGTExperiment", function(object) {
  callNextMethod()
  cat("experimentType:", experimentType(object), "\n")
  cat("fgtMetadata elements:", ifelse(length(fgtMetadata(object)) > 0, 
                               paste(names(fgtMetadata(object)), collapse = ", "), 
                               "none"), "\n")
})