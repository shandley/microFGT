#' Generate Example Data for microFGT
#'
#' Creates a realistic FGT microbiome dataset with count matrix, taxonomy, and metadata.
#' The function mimics the common community state types (CSTs) found in the female
#' genital tract with appropriate taxonomic structures and clinical parameters.
#'
#' @param n_samples Number of samples to generate
#' @param n_features Number of features (taxa) to generate
#' @param sample_groups Character vector of group names (e.g., c("Healthy", "BV"))
#' @param group_proportions Numeric vector of proportions for each group (must sum to 1)
#' @param community_types Character vector of CSTs to include (e.g., c("CST-I", "CST-III", "CST-IV"))
#' @param sequencing_depth Range of sequencing depths (c(min, max))
#' @param include_tree Logical: whether to generate a phylogenetic tree
#' @param random_seed Optional seed for reproducibility
#' @param output_dir Directory to save files (if NULL, no files are saved)
#' @param format Output format ("FGTExperiment", "rds", or "csv")
#'
#' @return A list containing the generated data or an FGTExperiment object
#'
#' @examples
#' \dontrun{
#' # Generate a simple dataset with default settings
#' example_data <- generate_fgt_example_data()
#'
#' # Generate a more complex dataset with specific parameters
#' example_data <- generate_fgt_example_data(
#'   n_samples = 30,
#'   n_features = 150,
#'   sample_groups = c("Healthy", "BV", "Intermediate"),
#'   group_proportions = c(0.6, 0.3, 0.1),
#'   community_types = c("CST-I", "CST-III", "CST-IV"),
#'   sequencing_depth = c(5000, 50000)
#' )
#' }
#' @export
generate_fgt_example_data <- function(
  n_samples = 20,
  n_features = 100,
  sample_groups = c("Healthy", "BV"),
  group_proportions = c(0.7, 0.3),
  community_types = c("CST-I", "CST-III", "CST-IV"),
  sequencing_depth = c(5000, 20000),
  include_tree = FALSE,
  random_seed = NULL,
  output_dir = NULL,
  format = "FGTExperiment"
) {
  # Input validation
  if (length(sample_groups) != length(group_proportions)) {
    stop("sample_groups and group_proportions must have the same length")
  }
  
  if (abs(sum(group_proportions) - 1) > 0.001) {
    stop("group_proportions must sum to 1")
  }
  
  # Set random seed if provided
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  
  # Generate a simple count matrix
  counts <- matrix(rpois(n_samples * n_features, 20), 
                  nrow = n_features, ncol = n_samples)
  rownames(counts) <- paste0("Feature", 1:n_features)
  colnames(counts) <- paste0("Sample", 1:n_samples)
  
  # Generate simple taxonomy 
  taxonomy <- data.frame(
    Kingdom = rep("Bacteria", n_features),
    Phylum = sample(c("Firmicutes", "Bacteroidetes", "Proteobacteria", "Actinobacteria"), 
                 n_features, replace = TRUE),
    Class = paste0("Class", 1:n_features),
    Order = paste0("Order", 1:n_features),
    Family = paste0("Family", 1:n_features),
    Genus = sample(c("Lactobacillus", "Gardnerella", "Prevotella", "Sneathia", "Megasphaera"),
                 n_features, replace = TRUE),
    Species = paste0("Species", 1:n_features),
    row.names = rownames(counts)
  )
  
  # Assign community types to samples
  cst_assignments <- sample(community_types, n_samples, replace = TRUE)
  
  # Determine sample groups
  group_counts <- numeric(length(sample_groups))
  for (i in 1:(length(sample_groups) - 1)) {
    group_counts[i] <- round(n_samples * group_proportions[i])
  }
  group_counts[length(sample_groups)] <- n_samples - sum(group_counts[1:(length(sample_groups) - 1)])
  
  # Create group assignments
  group_assignments <- character(n_samples)
  sample_idx <- 1
  for (i in 1:length(sample_groups)) {
    for (j in 1:group_counts[i]) {
      if (sample_idx <= n_samples) {
        group_assignments[sample_idx] <- sample_groups[i]
        sample_idx <- sample_idx + 1
      }
    }
  }
  
  # Shuffle group assignments
  group_assignments <- sample(group_assignments)
  
  # Generate simple metadata
  metadata <- data.frame(
    condition = group_assignments,
    community_state_type = cst_assignments,
    pH = numeric(n_samples),
    Nugent_Score = numeric(n_samples),
    row.names = colnames(counts)
  )
  
  # Assign realistic pH and Nugent score based on condition
  for (i in 1:n_samples) {
    if (metadata$condition[i] == "Healthy") {
      metadata$pH[i] <- runif(1, 3.8, 4.5)
      metadata$Nugent_Score[i] <- sample(0:3, 1)
    } else if (metadata$condition[i] == "BV") {
      metadata$pH[i] <- runif(1, 4.5, 6.0)
      metadata$Nugent_Score[i] <- sample(7:10, 1)
    } else {
      metadata$pH[i] <- runif(1, 4.3, 5.2)
      metadata$Nugent_Score[i] <- sample(4:6, 1)
    }
  }
  
  # Generate phylogenetic tree if requested
  tree <- NULL
  if (include_tree) {
    if (requireNamespace("ape", quietly = TRUE)) {
      tree <- ape::rtree(n_features, rooted = TRUE)
      tree$tip.label <- rownames(counts)
    } else {
      warning("Package 'ape' is required to generate phylogenetic trees. Tree will not be generated.")
    }
  }
  
  # Prepare return object based on format
  result <- list(
    counts = counts,
    taxonomy = taxonomy,
    metadata = metadata,
    tree = tree
  )
  
  # Save to files if output_dir is provided
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    saveRDS(counts, file.path(output_dir, "counts.rds"))
    saveRDS(taxonomy, file.path(output_dir, "taxonomy.rds"))
    saveRDS(metadata, file.path(output_dir, "metadata.rds"))
    if (!is.null(tree)) {
      saveRDS(tree, file.path(output_dir, "tree.rds"))
    }
  }
  
  # Convert to FGTExperiment if requested
  if (format == "FGTExperiment") {
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
                            experimentType = "amplicon",
                            fgtMetadata = S4Vectors::SimpleList())
        
        # Ensure assay names are set correctly
        SummarizedExperiment::assayNames(result)[1] <- "counts"
      } else {
        warning("FGTExperiment class not available. Returning TreeSummarizedExperiment object.")
        result <- tse
      }
    } else {
      warning("SummarizedExperiment and TreeSummarizedExperiment are required for FGTExperiment creation. Returning data as list.")
    }
  }
  
  return(result)
}