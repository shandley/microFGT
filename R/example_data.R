#' Generate Example Data for microFGT
#'
#' This file contains functions to generate realistic example data for testing,
#' documentation, and educational purposes. The generated data mimics real female
#' genital tract (FGT) microbiome profiles with community state types (CSTs) and
#' appropriate clinical metadata.
#'
#' @name example_data
NULL

# Load necessary internal constants
tryCatch({
  source(system.file("R/example_data/constants.R", package = "microFGT", mustWork = FALSE))
}, error = function(e) {
  # Constants will be defined directly in the test file
  message("Note: Constants are expected to be defined externally or already loaded")
})

#' Generate a complete example FGT microbiome dataset
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
#' @rdname example_data
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
  
  # Check community types
  valid_csts <- c("CST-I", "CST-II", "CST-III", "CST-IV", "CST-V")
  invalid_csts <- setdiff(community_types, valid_csts)
  if (length(invalid_csts) > 0) {
    stop("Invalid community types: ", paste(invalid_csts, collapse = ", "), 
         ". Valid types are: ", paste(valid_csts, collapse = ", "))
  }
  
  # Set random seed if provided
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  
  # Generate count matrix
  count_data <- create_fgt_count_matrix(
    n_samples = n_samples,
    n_features = n_features,
    community_types = community_types,
    sequencing_depth = sequencing_depth
  )
  
  # Generate taxonomy
  taxonomy <- create_fgt_taxonomy(
    feature_ids = rownames(count_data),
    community_types = community_types
  )
  
  # Generate sample metadata
  metadata <- create_fgt_sample_metadata(
    sample_ids = colnames(count_data),
    group_variable = "condition",
    groups = sample_groups,
    group_proportions = group_proportions,
    include_clinical = TRUE,
    community_types = community_types
  )
  
  # Generate phylogenetic tree if requested
  tree <- NULL
  if (include_tree) {
    if (requireNamespace("ape", quietly = TRUE)) {
      tree <- create_fgt_phylogenetic_tree(
        feature_ids = rownames(count_data),
        taxonomy_table = taxonomy
      )
    } else {
      warning("Package 'ape' is required to generate phylogenetic trees. Tree will not be generated.")
    }
  }
  
  # Prepare return object based on format
  result <- list(
    counts = count_data,
    taxonomy = taxonomy,
    metadata = metadata,
    tree = tree
  )
  
  # Save to files if output_dir is provided
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    save_example_data(
      seq_tab = count_data,
      taxonomy = taxonomy,
      metadata = metadata,
      tree = tree,
      output_dir = output_dir,
      base_name = "microFGT_example"
    )
  }
  
  # Convert to FGTExperiment if requested
  if (format == "FGTExperiment") {
    if (requireNamespace("SummarizedExperiment", quietly = TRUE) && 
        requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
      
      # Create using the two-step constructor approach
      se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(counts = count_data),
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

#' Create a count matrix with realistic FGT taxa distributions
#'
#' Generates a count matrix with realistic abundance patterns based on
#' community state types (CSTs) typically found in the female genital tract.
#'
#' @param n_samples Number of samples to generate
#' @param n_features Number of features (taxa) to generate
#' @param community_types Character vector of CSTs to include
#' @param community_proportions Numeric vector of proportions for each community type
#' @param sequencing_depth Range of sequencing depths (c(min, max))
#' @param random_seed Optional seed for reproducibility
#'
#' @return A count matrix with samples as columns and features as rows
#'
#' @keywords internal
create_fgt_count_matrix <- function(
  n_samples = 20,
  n_features = 100,
  community_types = c("CST-I", "CST-III", "CST-IV"),
  community_proportions = NULL,
  sequencing_depth = c(5000, 20000),
  random_seed = NULL
) {
  # Set random seed if provided
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  
  # Create equal proportions if not specified
  if (is.null(community_proportions)) {
    community_proportions <- rep(1/length(community_types), length(community_types))
  } else if (length(community_proportions) != length(community_types)) {
    community_proportions <- rep(1/length(community_types), length(community_types))
    warning("community_proportions must have the same length as community_types. Using equal proportions.")
  }
  
  # Normalize proportions to sum to 1
  community_proportions <- community_proportions / sum(community_proportions)
  
  # Determine how many samples per community type
  cst_counts <- numeric(length(community_types))
  remaining_samples <- n_samples
  
  for (i in 1:(length(community_types) - 1)) {
    cst_counts[i] <- round(n_samples * community_proportions[i])
    remaining_samples <- remaining_samples - cst_counts[i]
  }
  cst_counts[length(community_types)] <- remaining_samples
  
  # Generate a matrix of relative abundances first
  rel_abundance_matrix <- matrix(0, nrow = n_features, ncol = n_samples)
  
  # Generate feature names - a mix of known taxa and generated IDs
  feature_ids <- paste0("Feature", 1:n_features)
  rownames(rel_abundance_matrix) <- feature_ids
  
  # Generate sample names
  sample_ids <- paste0("Sample", 1:n_samples)
  colnames(rel_abundance_matrix) <- sample_ids
  
  # Assign community types to samples and generate relative abundances
  sample_idx <- 1
  for (cst_idx in 1:length(community_types)) {
    cst <- community_types[cst_idx]
    n_cst_samples <- cst_counts[cst_idx]
    
    # Get taxa template for this CST
    taxa_template <- switch(cst,
                          "CST-I" = CST_I_TAXA,
                          "CST-II" = CST_I_TAXA, # Temporary - use CST-I with modifications
                          "CST-III" = CST_III_TAXA,
                          "CST-IV" = CST_IV_TAXA,
                          "CST-V" = CST_V_TAXA,
                          CST_DEFAULT)
    
    # For each sample of this CST
    for (i in 1:n_cst_samples) {
      if (sample_idx <= n_samples) {
        # Initialize abundance vector
        abundances <- numeric(n_features)
        
        # Add dominant taxa
        for (taxon_info in taxa_template$dominant) {
          # Select a random taxon index (if this is a known taxon, we'll match it later)
          taxon_idx <- sample(1:n_features, 1)
          # Generate abundance within the specified range
          abundance <- runif(1, taxon_info$abundance[1], taxon_info$abundance[2])
          abundances[taxon_idx] <- abundance
        }
        
        # Add subdominant taxa
        for (taxon_info in taxa_template$subdominant) {
          # Select a random taxon index not already used
          available_indices <- which(abundances == 0)
          if (length(available_indices) > 0) {
            taxon_idx <- sample(available_indices, 1)
            # Generate abundance within the specified range
            abundance <- runif(1, taxon_info$abundance[1], taxon_info$abundance[2])
            abundances[taxon_idx] <- abundance
          }
        }
        
        # Add rare taxa
        n_rare_taxa <- min(length(taxa_template$rare), n_features - sum(abundances > 0))
        if (n_rare_taxa > 0) {
          rare_taxa_indices <- sample(which(abundances == 0), n_rare_taxa)
          for (j in 1:n_rare_taxa) {
            taxon_info <- taxa_template$rare[[j]]
            taxon_idx <- rare_taxa_indices[j]
            # Generate abundance within the specified range
            abundance <- runif(1, taxon_info$abundance[1], taxon_info$abundance[2])
            abundances[taxon_idx] <- abundance
          }
        }
        
        # Normalize to sum to 1
        total_abundance <- sum(abundances)
        if (total_abundance > 0) {
          abundances <- abundances / total_abundance
        } else {
          # If all abundances are 0, distribute evenly among features
          abundances <- rep(1/n_features, n_features)
        }
        
        # Assign to the matrix
        rel_abundance_matrix[, sample_idx] <- abundances
        sample_idx <- sample_idx + 1
      }
    }
  }
  
  # Convert relative abundances to counts using sequencing depths
  count_matrix <- matrix(0, nrow = nrow(rel_abundance_matrix), ncol = ncol(rel_abundance_matrix))
  rownames(count_matrix) <- rownames(rel_abundance_matrix)
  colnames(count_matrix) <- colnames(rel_abundance_matrix)
  
  for (i in 1:ncol(rel_abundance_matrix)) {
    # Generate a random sequencing depth
    depth <- round(runif(1, sequencing_depth[1], sequencing_depth[2]))
    # Convert relative abundances to counts
    count_matrix[, i] <- round(rel_abundance_matrix[, i] * depth)
  }
  
  return(count_matrix)
}

#' Create taxonomic classifications typical of FGT microbiome
#'
#' Generates taxonomic classifications for features, matching the community
#' state types typically found in the female genital tract.
#'
#' @param feature_ids Character vector of feature IDs
#' @param community_types Character vector of CSTs to include
#' @param include_rare_taxa Logical: whether to include rare taxa
#' @param taxonomic_levels Character vector of taxonomic levels to include
#'
#' @return A data frame with taxonomic classifications
#'
#' @keywords internal
create_fgt_taxonomy <- function(
  feature_ids,
  community_types = c("CST-I", "CST-III", "CST-IV"),
  include_rare_taxa = TRUE,
  taxonomic_levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
) {
  # Validate inputs
  if (is.null(feature_ids) || length(feature_ids) == 0) {
    stop("feature_ids must be provided")
  }
  
  # Check taxonomic levels
  invalid_levels <- setdiff(taxonomic_levels, TAXONOMIC_RANKS)
  if (length(invalid_levels) > 0) {
    warning("Unknown taxonomic levels: ", paste(invalid_levels, collapse = ", "))
  }
  
  # Create a data frame with the requested taxonomic levels
  tax_levels <- intersect(taxonomic_levels, TAXONOMIC_RANKS)
  n_features <- length(feature_ids)
  taxonomy <- data.frame(matrix(NA, nrow = n_features, ncol = length(tax_levels)))
  colnames(taxonomy) <- tax_levels
  rownames(taxonomy) <- feature_ids
  
  # Collect all taxa from the CSTs
  all_taxa <- list()
  
  for (cst in community_types) {
    taxa_template <- switch(cst,
                          "CST-I" = CST_I_TAXA,
                          "CST-II" = CST_I_TAXA, # Temporary, use CST-I
                          "CST-III" = CST_III_TAXA,
                          "CST-IV" = CST_IV_TAXA,
                          "CST-V" = CST_V_TAXA,
                          CST_DEFAULT)
    
    # Add dominant taxa
    for (taxon_info in taxa_template$dominant) {
      all_taxa[[length(all_taxa) + 1]] <- taxon_info$taxon
    }
    
    # Add subdominant taxa
    for (taxon_info in taxa_template$subdominant) {
      all_taxa[[length(all_taxa) + 1]] <- taxon_info$taxon
    }
    
    # Add rare taxa if requested
    if (include_rare_taxa) {
      for (taxon_info in taxa_template$rare) {
        all_taxa[[length(all_taxa) + 1]] <- taxon_info$taxon
      }
    }
  }
  
  # Convert to a matrix for easier manipulation
  if (length(all_taxa) > 0) {
    all_taxa_matrix <- do.call(rbind, all_taxa)
    
    # Remove duplicates
    all_taxa_matrix <- unique(all_taxa_matrix)
    
    # Assign known taxa to features
    n_known_taxa <- min(nrow(all_taxa_matrix), n_features)
    for (i in 1:n_known_taxa) {
      taxon <- all_taxa_matrix[i, ]
      for (j in 1:length(tax_levels)) {
        level_idx <- match(tax_levels[j], TAXONOMIC_RANKS)
        if (!is.na(level_idx) && level_idx <= length(taxon)) {
          taxonomy[i, j] <- taxon[level_idx]
        }
      }
    }
  }
  
  # Generate random taxa for any remaining features
  if (n_features > length(all_taxa)) {
    for (i in (length(all_taxa) + 1):n_features) {
      # Generate random taxonomic paths
      kingdom <- "Bacteria"
      phylum <- sample(c("Firmicutes", "Bacteroidetes", "Proteobacteria", "Actinobacteria", 
                         "Fusobacteria", "Tenericutes"), 1)
      class_name <- paste0(phylum, "_class", sample(1:5, 1))
      order_name <- paste0(class_name, "_order", sample(1:3, 1))
      family_name <- paste0(order_name, "_family", sample(1:3, 1))
      genus_name <- paste0(family_name, "_genus", sample(1:2, 1))
      species_name <- paste0(genus_name, "_sp", sample(1:3, 1))
      
      random_taxon <- c(kingdom, phylum, class_name, order_name, family_name, genus_name, species_name)
      
      # Add to taxonomy table
      for (j in 1:length(tax_levels)) {
        level_idx <- match(tax_levels[j], TAXONOMIC_RANKS)
        if (!is.na(level_idx) && level_idx <= length(random_taxon)) {
          taxonomy[i, j] <- random_taxon[level_idx]
        }
      }
    }
  }
  
  return(taxonomy)
}

#' Create clinical and technical metadata for FGT samples
#'
#' Generates realistic metadata for FGT microbiome samples, including clinical
#' parameters and technical information.
#'
#' @param sample_ids Character vector of sample IDs
#' @param group_variable Name of the grouping variable
#' @param groups Character vector of group names
#' @param group_proportions Numeric vector of proportions for each group
#' @param include_clinical Logical: whether to include clinical parameters
#' @param include_technical Logical: whether to include technical parameters
#' @param community_types Character vector of CSTs to include
#'
#' @return A data frame with sample metadata
#'
#' @keywords internal
create_fgt_sample_metadata <- function(
  sample_ids,
  group_variable = "condition",
  groups = c("Healthy", "BV"),
  group_proportions = c(0.7, 0.3),
  include_clinical = TRUE,
  include_technical = TRUE,
  community_types = NULL
) {
  # Validate inputs
  if (is.null(sample_ids) || length(sample_ids) == 0) {
    stop("sample_ids must be provided")
  }
  
  if (length(groups) != length(group_proportions)) {
    stop("groups and group_proportions must have the same length")
  }
  
  # Normalize proportions to sum to 1
  group_proportions <- group_proportions / sum(group_proportions)
  
  # Initialize metadata data frame
  n_samples <- length(sample_ids)
  metadata <- data.frame(row.names = sample_ids)
  
  # Assign groups to samples
  group_counts <- numeric(length(groups))
  remaining_samples <- n_samples
  
  for (i in 1:(length(groups) - 1)) {
    group_counts[i] <- round(n_samples * group_proportions[i])
    remaining_samples <- remaining_samples - group_counts[i]
  }
  group_counts[length(groups)] <- remaining_samples
  
  # Create group assignments
  group_assignments <- character(n_samples)
  sample_idx <- 1
  for (i in 1:length(groups)) {
    for (j in 1:group_counts[i]) {
      if (sample_idx <= n_samples) {
        group_assignments[sample_idx] <- groups[i]
        sample_idx <- sample_idx + 1
      }
    }
  }
  
  # Shuffle the assignments
  group_assignments <- sample(group_assignments)
  metadata[[group_variable]] <- group_assignments
  
  # Add community state types if provided
  if (!is.null(community_types) && length(community_types) > 0) {
    # Assign CSTs based on condition (with some randomness)
    cst_assignments <- character(n_samples)
    
    for (i in 1:n_samples) {
      condition <- metadata[i, group_variable]
      
      # Get compatible CSTs for this condition
      compatible_csts <- community_types
      if (condition %in% names(CST_CONDITION_MAP)) {
        compatible_csts <- intersect(community_types, 
                                    names(CST_CONDITION_MAP[CST_CONDITION_MAP == condition]))
      }
      
      # If no compatible CSTs, use any from the provided list
      if (length(compatible_csts) == 0) {
        compatible_csts <- community_types
      }
      
      # Assign a random CST from the compatible ones
      cst_assignments[i] <- sample(compatible_csts, 1)
    }
    
    metadata[["community_state_type"]] <- cst_assignments
  }
  
  # Add clinical parameters
  if (include_clinical) {
    # pH
    metadata[["pH"]] <- numeric(n_samples)
    # Nugent score
    metadata[["Nugent_Score"]] <- numeric(n_samples)
    # Amsel criteria sum (0-4)
    metadata[["Amsel_Score"]] <- numeric(n_samples)
    
    # Generate clinical parameters based on condition
    for (i in 1:n_samples) {
      condition <- metadata[i, group_variable]
      
      # Default to Healthy if condition not in CLINICAL_PARAMS
      if (!condition %in% names(CLINICAL_PARAMS$pH)) {
        condition <- "Healthy"
      }
      
      # pH
      ph_params <- CLINICAL_PARAMS$pH[[condition]]
      metadata[i, "pH"] <- min(max(rnorm(1, ph_params$mean, ph_params$sd), ph_params$min), ph_params$max)
      
      # Nugent score
      nugent_params <- CLINICAL_PARAMS$Nugent_Score[[condition]]
      metadata[i, "Nugent_Score"] <- round(min(max(rnorm(1, nugent_params$mean, nugent_params$sd), 
                                                 nugent_params$min), nugent_params$max))
      
      # Amsel score
      amsel_params <- CLINICAL_PARAMS$Amsel_Score[[condition]]
      metadata[i, "Amsel_Score"] <- round(min(max(rnorm(1, amsel_params$mean, amsel_params$sd), 
                                                amsel_params$min), amsel_params$max))
    }
    
    # Add vaginal pH category
    metadata[["pH_category"]] <- cut(metadata[["pH"]], 
                                   breaks = c(0, 4.5, 5.5, 10), 
                                   labels = c("Normal", "Intermediate", "High"))
    
    # Add BV diagnosis based on Nugent score
    metadata[["BV_Nugent"]] <- cut(metadata[["Nugent_Score"]], 
                                 breaks = c(-1, 3, 6, 10), 
                                 labels = c("Negative", "Intermediate", "Positive"))
    
    # Add BV diagnosis based on Amsel criteria
    metadata[["BV_Amsel"]] <- ifelse(metadata[["Amsel_Score"]] >= 3, "Positive", "Negative")
  }
  
  # Add technical parameters
  if (include_technical) {
    # Collection method
    collection_methods <- c("Swab", "Lavage", "Brush")
    metadata[["collection_method"]] <- sample(collection_methods, n_samples, replace = TRUE)
    
    # Sequencing batch (create 1-3 batches depending on sample size)
    n_batches <- max(1, min(3, round(n_samples / 10)))
    batch_ids <- paste0("Batch", 1:n_batches)
    metadata[["sequencing_batch"]] <- sample(batch_ids, n_samples, replace = TRUE)
    
    # Storage time (days)
    metadata[["storage_time"]] <- round(runif(n_samples, min = 1, max = 90))
    
    # DNA concentration (ng/uL)
    metadata[["DNA_concentration"]] <- runif(n_samples, min = 10, max = 50)
  }
  
  return(metadata)
}

#' Generate a phylogenetic tree matching the taxonomy
#'
#' Creates a phylogenetic tree that represents the taxonomic relationships
#' between features.
#'
#' @param feature_ids Character vector of feature IDs
#' @param taxonomy_table Data frame with taxonomic classifications
#' @param method Method for tree creation ("random" or "taxonomy-based")
#'
#' @return A phylo object representing the phylogenetic tree
#'
#' @keywords internal
create_fgt_phylogenetic_tree <- function(
  feature_ids,
  taxonomy_table,
  method = "taxonomy-based"
) {
  # Check if ape package is available
  if (!requireNamespace("ape", quietly = TRUE)) {
    stop("Package 'ape' is required to generate phylogenetic trees.")
  }
  
  # Validate inputs
  if (is.null(feature_ids) || length(feature_ids) == 0) {
    stop("feature_ids must be provided")
  }
  
  n_features <- length(feature_ids)
  
  if (method == "random") {
    # Create a random tree
    tree <- ape::rtree(n_features, rooted = TRUE)
    tree$tip.label <- feature_ids
    return(tree)
  } else if (method == "taxonomy-based") {
    # Create a taxonomy-based tree
    if (is.null(taxonomy_table) || nrow(taxonomy_table) == 0) {
      warning("taxonomy_table is empty, creating a random tree instead")
      tree <- ape::rtree(n_features, rooted = TRUE)
      tree$tip.label <- feature_ids
      return(tree)
    }
    
    # Create a distance matrix based on taxonomic similarity
    dist_mat <- matrix(0, nrow = n_features, ncol = n_features)
    rownames(dist_mat) <- feature_ids
    colnames(dist_mat) <- feature_ids
    
    # Calculate taxonomic distances
    for (i in 1:(n_features-1)) {
      for (j in (i+1):n_features) {
        # Count how many taxonomic levels match
        matches <- sum(taxonomy_table[i, ] == taxonomy_table[j, ], na.rm = TRUE)
        # Distance is inversely proportional to matches
        dist_mat[i, j] <- dist_mat[j, i] <- 1 - (matches / ncol(taxonomy_table))
      }
    }
    
    # Create a tree from the distance matrix
    tree <- ape::nj(as.dist(dist_mat))
    tree <- ape::root(tree, outgroup = 1, resolve.root = TRUE)
    
    return(tree)
  } else {
    stop("Unknown method: ", method, ". Valid methods are 'random' and 'taxonomy-based'.")
  }
}

#' Export generated data in various formats
#'
#' Saves the generated example data to files in the specified formats.
#'
#' @param seq_tab Count matrix
#' @param taxonomy Taxonomy table
#' @param metadata Sample metadata
#' @param tree Phylogenetic tree
#' @param output_dir Directory to save files
#' @param base_name Base name for the files
#' @param formats Formats to save ("rds", "csv", "newick")
#'
#' @return Invisibly returns paths to the saved files
#'
#' @keywords internal
save_example_data <- function(
  seq_tab,
  taxonomy,
  metadata,
  tree = NULL,
  output_dir = ".",
  base_name = "microFGT_example",
  formats = c("rds", "csv", "newick")
) {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialize list to store file paths
  files <- list()
  
  # Save in RDS format
  if ("rds" %in% formats) {
    count_file <- file.path(output_dir, paste0(base_name, "_counts.rds"))
    taxonomy_file <- file.path(output_dir, paste0(base_name, "_taxonomy.rds"))
    metadata_file <- file.path(output_dir, paste0(base_name, "_metadata.rds"))
    
    saveRDS(seq_tab, count_file)
    saveRDS(taxonomy, taxonomy_file)
    saveRDS(metadata, metadata_file)
    
    files$rds <- list(
      counts = count_file,
      taxonomy = taxonomy_file,
      metadata = metadata_file
    )
    
    if (!is.null(tree)) {
      tree_file <- file.path(output_dir, paste0(base_name, "_tree.rds"))
      saveRDS(tree, tree_file)
      files$rds$tree <- tree_file
    }
  }
  
  # Save in CSV format
  if ("csv" %in% formats) {
    count_file <- file.path(output_dir, paste0(base_name, "_counts.csv"))
    taxonomy_file <- file.path(output_dir, paste0(base_name, "_taxonomy.csv"))
    metadata_file <- file.path(output_dir, paste0(base_name, "_metadata.csv"))
    
    utils::write.csv(seq_tab, count_file)
    utils::write.csv(taxonomy, taxonomy_file)
    utils::write.csv(metadata, metadata_file)
    
    files$csv <- list(
      counts = count_file,
      taxonomy = taxonomy_file,
      metadata = metadata_file
    )
  }
  
  # Save tree in Newick format
  if ("newick" %in% formats && !is.null(tree) && requireNamespace("ape", quietly = TRUE)) {
    tree_file <- file.path(output_dir, paste0(base_name, "_tree.newick"))
    ape::write.tree(tree, tree_file)
    files$newick <- tree_file
  }
  
  invisible(files)
}

#' Load example data
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
#' @rdname example_data
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
    
    # Generate data
    example_data <- generate_fgt_example_data(
      n_samples = n_samples,
      n_features = n_features,
      sequencing_depth = c(5000, 20000),
      include_tree = TRUE,
      format = "list"
    )
    
    counts <- example_data$counts
    taxonomy <- example_data$taxonomy
    metadata <- example_data$metadata
    tree <- example_data$tree
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