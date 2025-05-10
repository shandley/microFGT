#!/usr/bin/env Rscript

# This script creates pre-built example datasets for microFGT
# Run this script during package development to generate example data files

# Load internal functions (assuming you're in the package root directory)
source("R/example_data.R")
source("R/example_data/constants.R")

# Create directories if they don't exist
if (!dir.exists("inst/extdata")) {
  dir.create("inst/extdata", recursive = TRUE)
}

# 1. Small amplicon dataset
set.seed(42)
small_amplicon <- generate_fgt_example_data(
  n_samples = 10,
  n_features = 50,
  sample_groups = c("Healthy", "BV"),
  group_proportions = c(0.7, 0.3),
  community_types = c("CST-I", "CST-III", "CST-IV"),
  sequencing_depth = c(5000, 20000),
  include_tree = TRUE,
  format = "list"
)

# Save to RDS files
saveRDS(small_amplicon$counts, "inst/extdata/microFGT_example_small_amplicon_counts.rds")
saveRDS(small_amplicon$taxonomy, "inst/extdata/microFGT_example_small_amplicon_taxonomy.rds")
saveRDS(small_amplicon$metadata, "inst/extdata/microFGT_example_small_amplicon_metadata.rds")
saveRDS(small_amplicon$tree, "inst/extdata/microFGT_example_small_amplicon_tree.rds")

# 2. Medium amplicon dataset
set.seed(43)
medium_amplicon <- generate_fgt_example_data(
  n_samples = 30,
  n_features = 150,
  sample_groups = c("Healthy", "BV", "Intermediate"),
  group_proportions = c(0.5, 0.3, 0.2),
  community_types = c("CST-I", "CST-II", "CST-III", "CST-IV", "CST-V"),
  sequencing_depth = c(10000, 50000),
  include_tree = TRUE,
  format = "list"
)

# Save to RDS files
saveRDS(medium_amplicon$counts, "inst/extdata/microFGT_example_medium_amplicon_counts.rds")
saveRDS(medium_amplicon$taxonomy, "inst/extdata/microFGT_example_medium_amplicon_taxonomy.rds")
saveRDS(medium_amplicon$metadata, "inst/extdata/microFGT_example_medium_amplicon_metadata.rds")
saveRDS(medium_amplicon$tree, "inst/extdata/microFGT_example_medium_amplicon_tree.rds")

# 3. Small metagenomic dataset (simplified for now - just rename amplicon)
set.seed(44)
small_metagenomic <- generate_fgt_example_data(
  n_samples = 10,
  n_features = 80,
  sample_groups = c("Healthy", "BV"),
  group_proportions = c(0.7, 0.3),
  community_types = c("CST-I", "CST-III", "CST-IV"),
  sequencing_depth = c(500000, 2000000),  # Higher depth for metagenomic
  include_tree = TRUE,
  format = "list"
)

# Save to RDS files
saveRDS(small_metagenomic$counts, "inst/extdata/microFGT_example_small_metagenomic_counts.rds")
saveRDS(small_metagenomic$taxonomy, "inst/extdata/microFGT_example_small_metagenomic_taxonomy.rds")
saveRDS(small_metagenomic$metadata, "inst/extdata/microFGT_example_small_metagenomic_metadata.rds")
saveRDS(small_metagenomic$tree, "inst/extdata/microFGT_example_small_metagenomic_tree.rds")

# Print summary of created datasets
cat("\nCreated example datasets:\n")
cat("1. Small amplicon dataset:", nrow(small_amplicon$counts), "features,", ncol(small_amplicon$counts), "samples\n")
cat("2. Medium amplicon dataset:", nrow(medium_amplicon$counts), "features,", ncol(medium_amplicon$counts), "samples\n")
cat("3. Small metagenomic dataset:", nrow(small_metagenomic$counts), "features,", ncol(small_metagenomic$counts), "samples\n")

cat("\nExample datasets created successfully and saved to inst/extdata/\n")