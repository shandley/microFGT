#!/usr/bin/env Rscript

# Simple test script for microFGT
library(microFGT)

# Create a simple dataset
set.seed(42)
counts <- matrix(rpois(100, 20), nrow=10, ncol=10)
rownames(counts) <- paste0("OTU", 1:10)
colnames(counts) <- paste0("Sample", 1:10)

# Create a simple taxonomy table
taxa <- data.frame(
  Kingdom = rep("Bacteria", 10),
  Phylum = sample(c("Firmicutes", "Bacteroidetes", "Proteobacteria"), 10, replace=TRUE),
  row.names = rownames(counts)
)

# Create sample metadata
metadata <- data.frame(
  Group = rep(c("A", "B"), each=5),
  row.names = colnames(counts)
)

# List available functions in the package
cat("\nAvailable functions in microFGT package:\n")
package_fns <- ls("package:microFGT")
print(package_fns)

# Create an FGTExperiment object
cat("\nCreating FGTExperiment object...\n")
fgt <- FGTExperiment(
  assays = list(counts = counts),
  rowData = taxa,
  colData = metadata
)

# Print information about the object
cat("\nFGTExperiment object information:\n")
print(fgt)

# Test a data transformation if available
if ("transform_abundance" %in% package_fns) {
  cat("\nTesting transform_abundance...\n")
  fgt_rel <- transform_abundance(fgt, method = "relative")
  cat("First few values of transformed data:\n")
  print(head(assays(fgt_rel)$counts))
} else {
  cat("\ntransform_abundance function not found\n")
}

cat("\nTest completed!\n")