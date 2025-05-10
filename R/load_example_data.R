#' Load Example Data for microFGT
#'
#' Loads pre-generated example datasets for testing and demonstrations.
#'
#' @param size Size of the dataset ("small", "medium", "large")
#' @param type Type of data ("amplicon", "metagenomics")
#' @param as_fgt_experiment Logical: whether to return an FGTExperiment object
#'
#' @return A list containing the data or an FGTExperiment object
#'
#' @examples
#' \dontrun{
#' # Load a small amplicon dataset
#' data <- load_example_data("small", "amplicon")
#'
#' # Load a medium dataset as an FGTExperiment object
#' fgt_exp <- load_example_data("medium", "amplicon", as_fgt_experiment = TRUE)
#' }
#' @export
load_example_data <- function(
  size = "small",
  type = "amplicon",
  as_fgt_experiment = TRUE
) {
  # Validate inputs
  size <- match.arg(size, c("small", "medium", "large"))
  type <- match.arg(type, c("amplicon", "metagenomics"))
  
  # Construct the file path
  base_name <- paste0("microFGT_example_", size, "_", type)
  
  # Check if we have pre-built examples
  data_dir <- system.file("extdata", package = "microFGT")
  
  if (file.exists(file.path(data_dir, paste0(base_name, "_counts.rds")))) {
    # Load pre-built example
    counts <- readRDS(file.path(data_dir, paste0(base_name, "_counts.rds")))
    taxonomy <- readRDS(file.path(data_dir, paste0(base_name, "_taxonomy.rds")))
    metadata <- readRDS(file.path(data_dir, paste0(base_name, "_metadata.rds")))
    
    # Check if tree exists
    tree <- NULL
    tree_file <- file.path(data_dir, paste0(base_name, "_tree.rds"))
    if (file.exists(tree_file)) {
      tree <- readRDS(tree_file)
    }
  } else {
    # Generate example data on the fly
    message("Pre-built example not found. Generating example data on the fly.")
    
    # Set parameters based on size
    n_samples <- switch(size,
                       "small" = 10,
                       "medium" = 30,
                       "large" = 100)
    
    n_features <- switch(size,
                        "small" = 50,
                        "medium" = 150,
                        "large" = 500)
    
    # Generate data - We'd call generate_fgt_example_data here but to avoid
    # dependencies for this basic functionality, we'll create a simple dataset
    
    # Create a simple count matrix
    counts <- matrix(rpois(n_samples * n_features, 20), 
                    nrow = n_features, ncol = n_samples)
    rownames(counts) <- paste0("Feature", 1:n_features)
    colnames(counts) <- paste0("Sample", 1:n_samples)
    
    # Create simple taxonomy
    taxonomy <- data.frame(
      Kingdom = rep("Bacteria", n_features),
      Phylum = sample(c("Firmicutes", "Bacteroidetes", "Proteobacteria"), 
                     n_features, replace = TRUE),
      row.names = rownames(counts)
    )
    
    # Create simple metadata
    metadata <- data.frame(
      condition = sample(c("Healthy", "BV"), n_samples, replace = TRUE),
      pH = runif(n_samples, 3.8, 5.5),
      row.names = colnames(counts)
    )
    
    # No tree generated on the fly
    tree <- NULL
  }
  
  # Return as list or FGTExperiment
  if (as_fgt_experiment) {
    if (requireNamespace("SummarizedExperiment", quietly = TRUE) && 
        requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
      
      # Create using the two-step constructor approach
      se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = counts),
        rowData = taxonomy,
        colData = metadata
      )
      
      tse <- TreeSummarizedExperiment::TreeSummarizedExperiment(se, rowTree = tree)
      
      if (methods::existsClass("FGTExperiment")) {
        result <- methods::new("FGTExperiment", 
                            tse,
                            experimentType = type,
                            fgtMetadata = S4Vectors::SimpleList())
        
        # Ensure assay names are set correctly
        SummarizedExperiment::assayNames(result)[1] <- "counts"
        return(result)
      } else {
        warning("FGTExperiment class not available. Returning TreeSummarizedExperiment object.")
        return(tse)
      }
    } else {
      warning("SummarizedExperiment and TreeSummarizedExperiment are required for FGTExperiment creation. Returning data as list.")
    }
  }
  
  # Return as list
  return(list(
    counts = counts,
    taxonomy = taxonomy,
    metadata = metadata,
    tree = tree
  ))
}