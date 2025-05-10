#!/usr/bin/env Rscript

# Fixed example for microFGT
cat("Fixed example for microFGT constructor\n\n")

library(microFGT)

# Create minimal matrix with EXACT dimensions properly labeled
counts <- matrix(1:50, nrow=10, ncol=5)
rownames(counts) <- paste0("Feature", 1:10)
colnames(counts) <- paste0("Sample", 1:5)

# Print matrix information
cat("Matrix dimensions:", dim(counts), "\n")
cat("Has rownames:", !is.null(rownames(counts)), "\n")
cat("Has colnames:", !is.null(colnames(counts)), "\n\n")

# Try a direct approach with hard-coded row and column data
feature_data <- data.frame(row.names = rownames(counts))
sample_data <- data.frame(row.names = colnames(counts))

# Try a direct SummarizedExperiment approach first
if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
  cat("Creating SummarizedExperiment...\n")
  tryCatch({
    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = counts),
      rowData = feature_data,
      colData = sample_data
    )
    cat("SummarizedExperiment created successfully\n")
    print(se)
    
    # Try creating FGTExperiment from the SummarizedExperiment
    cat("\nTrying to create FGTExperiment from SE...\n")
    tryCatch({
      # Use the internal implementation of new to create the object directly
      fgt <- methods::new("FGTExperiment", 
                         se,
                         experimentType = "amplicon",
                         fgtMetadata = S4Vectors::SimpleList())
      
      cat("FGTExperiment created successfully via new()\n")
      print(fgt)
    }, error = function(e) {
      cat("Error creating FGTExperiment from SE:", conditionMessage(e), "\n")
    })
  }, error = function(e) {
    cat("Error creating SummarizedExperiment:", conditionMessage(e), "\n")
  })
}