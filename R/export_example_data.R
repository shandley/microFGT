#' Export Example Data to Common Bioinformatics Formats
#'
#' This function exports microFGT example data to formats compatible with
#' common bioinformatics tools like phyloseq, DADA2, or QIIME2.
#'
#' @param data Example data (list or FGTExperiment object)
#' @param output_dir Directory to save the exported files
#' @param format Format to export to ("phyloseq", "dada2", "qiime2")
#'
#' @return Invisibly returns a list of the exported file paths
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' example_data <- generate_fgt_example_data()
#'
#' # Export to DADA2 format
#' export_example_data(example_data, tempdir(), format = "dada2")
#' }
#' @export
export_example_data <- function(data, output_dir, format = c("phyloseq", "dada2", "qiime2")) {
  format <- match.arg(format)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract components from FGTExperiment if needed
  if (methods::is(data, "FGTExperiment")) {
    counts <- SummarizedExperiment::assays(data)$counts
    taxonomy <- SummarizedExperiment::rowData(data)
    metadata <- SummarizedExperiment::colData(data)
    tree <- NULL
    if (methods::is(data, "TreeSummarizedExperiment")) {
      tree <- TreeSummarizedExperiment::rowTree(data)
    }
  } else if (is.list(data)) {
    counts <- data$counts
    taxonomy <- data$taxonomy
    metadata <- data$metadata
    tree <- data$tree
  } else {
    stop("data must be an FGTExperiment object or a list with counts, taxonomy, and metadata components")
  }
  
  # Save files based on format
  base_name <- "microFGT_export"
  
  # Save counts
  counts_file <- file.path(output_dir, paste0(base_name, "_counts.csv"))
  utils::write.csv(counts, counts_file)
  
  # Save taxonomy
  tax_file <- file.path(output_dir, paste0(base_name, "_taxonomy.csv"))
  utils::write.csv(taxonomy, tax_file)
  
  # Save metadata
  meta_file <- file.path(output_dir, paste0(base_name, "_metadata.csv"))
  utils::write.csv(metadata, meta_file)
  
  # Save tree if available
  tree_file <- NULL
  if (!is.null(tree) && requireNamespace("ape", quietly = TRUE)) {
    tree_file <- file.path(output_dir, paste0(base_name, "_tree.newick"))
    ape::write.tree(tree, tree_file)
  }
  
  # Return file paths
  result <- list(
    counts = counts_file,
    taxonomy = tax_file,
    metadata = meta_file
  )
  
  if (!is.null(tree_file)) {
    result$tree <- tree_file
  }
  
  message("Files exported to: ", output_dir)
  return(invisible(result))
}