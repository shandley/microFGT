#!/usr/bin/env Rscript

# Test the fixed FGTExperiment constructor
library(microFGT)
source("/Users/scotthandley/Code/microFGT/microFGT/R/fixed_constructor.R")

# Create test data
set.seed(42)
counts <- matrix(rpois(50, 20), nrow=10, ncol=5)
rownames(counts) <- paste0("Feature", 1:10)
colnames(counts) <- paste0("Sample", 1:5)

# Try the original constructor first
cat("Testing original FGTExperiment constructor...\n")
result <- tryCatch({
  fgt <- FGTExperiment(assays = list(counts = counts))
  cat("Original constructor worked!\n")
  print(fgt)
  TRUE
}, error = function(e) {
  cat("Original constructor failed:", conditionMessage(e), "\n")
  FALSE
})

# Try the fixed constructor
cat("\nTesting fixed createFGTExperiment constructor...\n")
result <- tryCatch({
  fgt_fixed <- createFGTExperiment(assays = list(counts = counts))
  cat("Fixed constructor worked!\n")
  print(fgt_fixed)
  TRUE
}, error = function(e) {
  cat("Fixed constructor failed:", conditionMessage(e), "\n")
  FALSE
})

# Testing data manipulation
if (exists("fgt_fixed") && is(fgt_fixed, "FGTExperiment")) {
  cat("\nTesting data manipulation with fixed constructor...\n")
  
  # Try to transform data
  tryCatch({
    if ("transform_abundance" %in% ls("package:microFGT")) {
      cat("Testing transform_abundance...\n")
      fgt_rel <- transform_abundance(fgt_fixed, method = "relative")
      cat("First few values of transformed data:\n")
      print(head(assays(fgt_rel)$counts, 3))
    }
  }, error = function(e) {
    cat("Error in transform_abundance:", conditionMessage(e), "\n")
  })
}

cat("\nTest completed!\n")