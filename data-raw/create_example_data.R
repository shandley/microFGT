#!/usr/bin/env Rscript

# Script to create and save example datasets for the microFGT package
# These datasets are used in documentation, tests, and for user experiments

library(microFGT)

# Set seed for reproducibility
set.seed(42)

# Create a small example dataset
small_example <- generate_fgt_example_data(
  n_samples = 8,
  n_features = 30,
  sample_groups = c("Healthy", "BV"),
  group_proportions = c(0.7, 0.3),
  community_types = c("CST-I", "CST-III", "CST-IV"),
  format = "list"
)

# Save as RDS files in inst/extdata
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

saveRDS(small_example$counts, file = "inst/extdata/microFGT_example_small_amplicon_counts.rds")
saveRDS(small_example$taxonomy, file = "inst/extdata/microFGT_example_small_amplicon_taxonomy.rds")
saveRDS(small_example$metadata, file = "inst/extdata/microFGT_example_small_amplicon_metadata.rds")
saveRDS(small_example$tree, file = "inst/extdata/microFGT_example_small_amplicon_tree.rds")

# Optional: Create a medium-sized example
set.seed(43)
medium_example <- generate_fgt_example_data(
  n_samples = 20,
  n_features = 100,
  sample_groups = c("Healthy", "BV", "Intermediate"),
  group_proportions = c(0.5, 0.3, 0.2),
  community_types = c("CST-I", "CST-II", "CST-III", "CST-IV", "CST-V"),
  format = "list"
)

# Save medium example
saveRDS(medium_example$counts, file = "inst/extdata/microFGT_example_medium_amplicon_counts.rds")
saveRDS(medium_example$taxonomy, file = "inst/extdata/microFGT_example_medium_amplicon_taxonomy.rds")
saveRDS(medium_example$metadata, file = "inst/extdata/microFGT_example_medium_amplicon_metadata.rds")
saveRDS(medium_example$tree, file = "inst/extdata/microFGT_example_medium_amplicon_tree.rds")

# Print summary
cat("Example datasets created and saved to inst/extdata/\n")
cat("Small dataset:", nrow(small_example$counts), "features,", 
    ncol(small_example$counts), "samples\n")
cat("Medium dataset:", nrow(medium_example$counts), "features,", 
    ncol(medium_example$counts), "samples\n")