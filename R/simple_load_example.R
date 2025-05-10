#' Simple Example Data Loader
#'
#' A simplified function to load example data without complex dependencies.
#' This function provides a reliable way to load pre-built example datasets
#' with minimal dependencies.
#'
#' @param as_fgt_experiment Logical: whether to convert to FGTExperiment (default TRUE)
#'
#' @return A FGTExperiment object or a list with components (counts, taxonomy, metadata, tree)
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data <- simple_load_example()
#'
#' # Examine the object
#' data
#' 
#' # Access count data
#' head(assays(data)$counts)
#' }
#' @export
simple_load_example <- function(as_fgt_experiment = TRUE) {
  # Get the data path
  data_dir <- system.file("extdata", package = "microFGT")
  
  if (data_dir == "") {
    stop("Package 'microFGT' not properly installed or example data not found.")
  }
  
  # Check that files exist
  count_file <- file.path(data_dir, "microFGT_example_small_amplicon_counts.rds")
  tax_file <- file.path(data_dir, "microFGT_example_small_amplicon_taxonomy.rds")
  meta_file <- file.path(data_dir, "microFGT_example_small_amplicon_metadata.rds")
  tree_file <- file.path(data_dir, "microFGT_example_small_amplicon_tree.rds")
  
  if (!file.exists(count_file) || !file.exists(tax_file) || 
      !file.exists(meta_file) || !file.exists(tree_file)) {
    stop("Example data files not found in package.")
  }
  
  # Load the files
  counts <- readRDS(count_file)
  taxonomy <- readRDS(tax_file)
  metadata <- readRDS(meta_file)
  tree <- readRDS(tree_file)
  
  # Check if we need to return as FGTExperiment
  if (!as_fgt_experiment) {
    return(list(
      counts = counts,
      taxonomy = taxonomy, 
      metadata = metadata,
      tree = tree
    ))
  }
  
  # Try to load required packages
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    warning("Package 'SummarizedExperiment' not available. Returning list.")
    return(list(counts = counts, taxonomy = taxonomy, metadata = metadata, tree = tree))
  }
  
  if (!requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
    warning("Package 'TreeSummarizedExperiment' not available. Returning list.")
    return(list(counts = counts, taxonomy = taxonomy, metadata = metadata, tree = tree))
  }
  
  if (!requireNamespace("S4Vectors", quietly = TRUE)) {
    warning("Package 'S4Vectors' not available. Returning list.")
    return(list(counts = counts, taxonomy = taxonomy, metadata = metadata, tree = tree))
  }
  
  # Create a SummarizedExperiment
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts),
    rowData = taxonomy,
    colData = metadata
  )
  
  # Convert to TreeSummarizedExperiment
  tse <- TreeSummarizedExperiment::TreeSummarizedExperiment(se, rowTree = tree)
  
  # Try to convert to FGTExperiment
  has_fgt <- tryCatch({
    is(tse, "TreeSummarizedExperiment") && 
    isClass("FGTExperiment") && 
    canCoerce(tse, "FGTExperiment")
  }, error = function(e) FALSE)
  
  if (has_fgt) {
    # Try to create FGTExperiment
    fgt <- tryCatch({
      obj <- new("FGTExperiment", 
               tse,
               experimentType = "amplicon",
               fgtMetadata = S4Vectors::SimpleList())
      
      # Set assay names
      SummarizedExperiment::assayNames(obj)[1] <- "counts"
      obj
    }, error = function(e) {
      warning("Error creating FGTExperiment: ", conditionMessage(e), 
              "\nReturning TreeSummarizedExperiment.")
      tse
    })
    
    return(fgt)
  } else {
    # Return TreeSummarizedExperiment as fallback
    message("FGTExperiment class not available. Returning TreeSummarizedExperiment.")
    return(tse)
  }
}