#!/usr/bin/env Rscript

# Minimal test script for microFGT
library(microFGT)

# Print all exported functions 
cat("Exported functions in microFGT:\n")
print(ls("package:microFGT"))

# Create a minimal properly-formed dataset
set.seed(42)
n_samples <- 5
n_features <- 8

# Create count matrix with proper row and column names
counts <- matrix(rpois(n_features * n_samples, 20), 
                nrow = n_features, 
                ncol = n_samples)
rownames(counts) <- paste0("Feature", 1:n_features)
colnames(counts) <- paste0("Sample", 1:n_samples)

# Verify matrix dimensions and names
cat("\nCounts matrix dimensions:", dim(counts), "\n")
cat("First few rownames:", head(rownames(counts)), "\n")
cat("First few colnames:", head(colnames(counts)), "\n")

# Debug TreeSummarizedExperiment constructor directly
cat("\nTesting TreeSummarizedExperiment constructor...\n")
tryCatch({
  if (requireNamespace("TreeSummarizedExperiment", quietly = TRUE)) {
    tse <- TreeSummarizedExperiment::TreeSummarizedExperiment(
      assays = list(counts = counts)
    )
    cat("TSE created successfully\n")
    print(tse)
  } else {
    cat("TreeSummarizedExperiment package not available\n")
  }
}, error = function(e) {
  cat("Error creating TSE:", conditionMessage(e), "\n")
})

# Try with SummarizedExperiment first
cat("\nTesting SummarizedExperiment constructor...\n")
tryCatch({
  if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = counts)
    )
    cat("SE created successfully\n")
    print(se)
  } else {
    cat("SummarizedExperiment package not available\n")
  }
}, error = function(e) {
  cat("Error creating SE:", conditionMessage(e), "\n")
})

# Now try FGTExperiment constructor
cat("\nTesting FGTExperiment constructor...\n")
tryCatch({
  fgt <- FGTExperiment(
    assays = list(counts = counts)
  )
  cat("FGTExperiment created successfully\n")
  print(fgt)
}, error = function(e) {
  cat("Error creating FGTExperiment:", conditionMessage(e), "\n")
})

# Print the session information for debugging
cat("\nSession info:\n")
print(sessionInfo())