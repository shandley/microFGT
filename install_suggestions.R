#!/usr/bin/env Rscript

# Script to install microFGT suggested packages
# Run with: Rscript install_suggestions.R

# Install BiocManager if not already installed
if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")

# Set repositories and install BiocManager
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")

# Install Bioconductor dependencies with specific version
message("Installing dada2 and its dependencies...")
BiocManager::install("dada2", update = FALSE, ask = FALSE)

message("Installing DESeq2...")
BiocManager::install("DESeq2", update = FALSE, ask = FALSE)

message("Installing phyloseq...")
BiocManager::install("phyloseq", update = FALSE, ask = FALSE)

message("Installing remaining suggested packages...")
packages <- c(
  "Rsamtools", 
  "GenomicAlignments", 
  "ShortRead", 
  "vegan"
)
BiocManager::install(packages, update = FALSE, ask = FALSE)

# Verify installations
installed_packages <- rownames(installed.packages())
suggested_packages <- c("dada2", "DESeq2", "phyloseq", "Rsamtools", 
                        "GenomicAlignments", "ShortRead", "vegan")

message("\nInstallation status:")
for (pkg in suggested_packages) {
  status <- if (pkg %in% installed_packages) "Installed" else "Not installed"
  message(sprintf("- %s: %s", pkg, status))
}

message("\nIf any packages failed to install, try installing them individually with:")
message('BiocManager::install("package_name", dependencies = TRUE)')