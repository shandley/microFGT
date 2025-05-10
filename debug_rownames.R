#!/usr/bin/env Rscript

# Script to debug the rownames error in microFGT
cat("Debugging rownames error in microFGT\n\n")

# Create minimal example with explicit dimensions and names
n_features <- 10
n_samples <- 5

# 1. Create and verify matrix
counts <- matrix(1:50, nrow=n_features, ncol=n_samples)
rownames(counts) <- paste0("Feature", 1:n_features)
colnames(counts) <- paste0("Sample", 1:n_samples)

cat("Counts matrix created:\n")
cat("- Dimensions:", dim(counts), "\n")
cat("- Rownames:", paste(rownames(counts)[1:3], "..."), "\n")
cat("- Colnames:", paste(colnames(counts), collapse=", "), "\n\n")

# 2. Verify if BiocGenerics is loaded correctly
cat("BiocGenerics availability:\n")
bioGenerics_loaded <- requireNamespace("BiocGenerics", quietly=TRUE)
cat("- BiocGenerics loaded:", ifelse(bioGenerics_loaded, "Yes", "No"), "\n")
if (bioGenerics_loaded) {
  cat("- BiocGenerics version:", as.character(packageVersion("BiocGenerics")), "\n\n")
}

# 3. Check if the core S4Vector functions are working
cat("S4Vectors availability:\n")
s4vectors_loaded <- requireNamespace("S4Vectors", quietly=TRUE)
cat("- S4Vectors loaded:", ifelse(s4vectors_loaded, "Yes", "No"), "\n")
if (s4vectors_loaded) {
  cat("- S4Vectors version:", as.character(packageVersion("S4Vectors")), "\n\n")
  
  # Test if SimpleList constructor works
  tryCatch({
    test_simple_list <- S4Vectors::SimpleList(a=1)
    cat("- SimpleList test: Success\n\n")
  }, error=function(e) {
    cat("- SimpleList test: Failed -", conditionMessage(e), "\n\n")
  })
}

# 4. Check if SummarizedExperiment is working
cat("SummarizedExperiment test:\n")
summarized_exp_loaded <- requireNamespace("SummarizedExperiment", quietly=TRUE)
cat("- SummarizedExperiment loaded:", ifelse(summarized_exp_loaded, "Yes", "No"), "\n")
if (summarized_exp_loaded) {
  cat("- SummarizedExperiment version:", as.character(packageVersion("SummarizedExperiment")), "\n")
  
  # Try to create a basic SummarizedExperiment
  tryCatch({
    se <- SummarizedExperiment::SummarizedExperiment(assays=list(counts=counts))
    cat("- SummarizedExperiment creation: Success\n")
    cat("- SE dimensions:", dim(se), "\n\n")
  }, error=function(e) {
    cat("- SummarizedExperiment creation: Failed -", conditionMessage(e), "\n\n")
  })
}

# 5. Check if TreeSummarizedExperiment is working
cat("TreeSummarizedExperiment test:\n")
tree_se_loaded <- requireNamespace("TreeSummarizedExperiment", quietly=TRUE)
cat("- TreeSummarizedExperiment loaded:", ifelse(tree_se_loaded, "Yes", "No"), "\n")
if (tree_se_loaded) {
  cat("- TreeSummarizedExperiment version:", as.character(packageVersion("TreeSummarizedExperiment")), "\n")
  
  # Try to create a basic TreeSummarizedExperiment
  tryCatch({
    tse <- TreeSummarizedExperiment::TreeSummarizedExperiment(assays=list(counts=counts))
    cat("- TreeSummarizedExperiment creation: Success\n")
    cat("- TSE dimensions:", dim(tse), "\n\n")
  }, error=function(e) {
    cat("- TreeSummarizedExperiment creation: Failed -", conditionMessage(e), "\n\n")
  })
}

# 6. Now try FGTExperiment
cat("microFGT test:\n")
cat("- microFGT version:", as.character(packageVersion("microFGT")), "\n")

# Try to access the FGTExperiment constructor function
fgt_exp_exists <- exists("FGTExperiment", mode="function")
cat("- FGTExperiment constructor exists:", ifelse(fgt_exp_exists, "Yes", "No"), "\n")

# Try to create an FGTExperiment
tryCatch({
  # Trace execution to see where it fails
  if ("FGTExperiment" %in% ls("package:microFGT")) {
    cat("- Starting FGTExperiment constructor call...\n")
    
    # Try a more direct approach bypassing the constructor
    fgt <- try(FGTExperiment(assays=list(counts=counts)))
    
    cat("- FGTExperiment creation:", ifelse(class(fgt)[1] != "try-error", "Success", "Failed"), "\n")
    if (class(fgt)[1] != "try-error") {
      cat("- FGT dimensions:", dim(fgt), "\n")
    }
  } else {
    cat("- FGTExperiment function not found in package namespace\n")
  }
}, error=function(e) {
  cat("- FGTExperiment creation: Failed -", conditionMessage(e), "\n")
})

# Print session info
cat("\nSession Info:\n")
print(sessionInfo())