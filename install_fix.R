#!/usr/bin/env Rscript

# Script to reinstall critical dependencies for microFGT
cat("Reinstalling critical dependencies for microFGT\n\n")

# BiocManager should be installed first
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  cat("Installing BiocManager...\n")
  install.packages("BiocManager")
} else {
  cat("BiocManager is already installed, version:", 
      as.character(packageVersion("BiocManager")), "\n")
}

# Set repositories and install core dependencies with specific version
cat("\nInstalling core Bioconductor dependencies...\n")
BiocManager::install(c("BiocGenerics", "S4Vectors", "IRanges", 
                       "BiocParallel", "GenomeInfoDb", "SummarizedExperiment",
                       "TreeSummarizedExperiment"), 
                     update = FALSE, ask = FALSE)

# Check if installations succeeded
cat("\nChecking installed packages:\n")
required_pkgs <- c("BiocGenerics", "S4Vectors", "IRanges", "BiocParallel",
                   "GenomeInfoDb", "SummarizedExperiment", "TreeSummarizedExperiment")
                   
for (pkg in required_pkgs) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("- ", pkg, ": Installed, version ", 
        as.character(packageVersion(pkg)), "\n", sep="")
  } else {
    cat("- ", pkg, ": NOT installed\n", sep="")
  }
}

# Reinstall microFGT from GitHub
cat("\nReinstalling microFGT from GitHub...\n")
if (!requireNamespace("devtools", quietly = TRUE)) {
  cat("Installing devtools...\n")
  install.packages("devtools")
}

cat("Installing microFGT from GitHub...\n")
devtools::install_github("shandley/microFGT", dependencies = TRUE)