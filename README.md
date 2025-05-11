# microFGT: Comprehensive Analysis of Female Genital Tract Microbiome Data

<img src="inst/extdata/microFGT_logo.png" alt="microFGT Logo" width="150" align="right"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/shandley/microFGT/workflows/R-CMD-check/badge.svg)](https://github.com/shandley/microFGT/actions)
[![Codecov test coverage](https://codecov.io/gh/shandley/microFGT/branch/main/graph/badge.svg)](https://codecov.io/gh/shandley/microFGT?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

microFGT is an integrated R package for comprehensive analysis of female genital tract (FGT) microbiome data. This package unifies specialized tools (dada2, phyloseq, speciateIT, VALENCIA, and VIRGO) within a cohesive framework that handles both amplicon and metagenomic sequencing data.

> 🚧 **ALPHA STAGE SOFTWARE**: This package is in early development and not ready for production use. APIs may change, functions may not work as expected, and documentation may be incomplete.

## Features

- Built on TreeSummarizedExperiment data structure
- Specialized `FGTExperiment` class for FGT microbiome analysis
- Community State Type (CST) identification and analysis 
- Supports both amplicon and metagenomic data
- Includes realistic example datasets for FGT microbiomes
- Simulates CSTs with appropriate taxonomic profiles
- Compatible with Bioconductor ecosystem
- Follows tidyverse design principles

## Installation

```r
# Install dependencies from CRAN
install.packages(c("dplyr", "tibble", "magrittr", "ggplot2", "Rcpp", "methods"))

# Install Bioconductor dependencies
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")

# Add BioConductor repositories and install dependencies
BiocManager::install(c("TreeSummarizedExperiment", "SummarizedExperiment",
                      "MultiAssayExperiment", "S4Vectors", "Biostrings", "BiocParallel"),
                    update = FALSE)

# Install microFGT from GitHub
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
devtools::install_github("shandley/microFGT", dependencies = TRUE)
```

## Getting Started

### Using Example Data

microFGT includes built-in example data that simulates realistic FGT microbiome profiles. This is a great way to explore the package's functionality without needing your own data:

```r
library(microFGT)

# Load pre-built example dataset (returns an FGTExperiment object)
fgt_exp <- load_example_data(size = "small", type = "amplicon")

# View basic information about the dataset
fgt_exp

# See sample metadata
colData(fgt_exp)[, c("condition", "pH", "Nugent_Score")]

# Transform to relative abundance
fgt_rel <- transform_abundance(fgt_exp, type = "relative", assay_name = "counts")

# Plot taxonomic composition
plot_taxa_composition(fgt_rel, rank = "Phylum", top_n = 5, group_var = "condition")

# Calculate and plot alpha diversity
plot_alpha_diversity(fgt_exp, metrics = c("shannon"), group_var = "condition")
```

### Generate Custom Example Data

You can also generate custom example data with specific properties:

```r
# Generate a dataset with specific community state types
custom_data <- generate_fgt_example_data(
  n_samples = 20,
  n_features = 100,
  sample_groups = c("Healthy", "BV", "Intermediate"),
  group_proportions = c(0.5, 0.3, 0.2),
  community_types = c("CST-I", "CST-III", "CST-IV"),
  format = "FGTExperiment"
)

# Explore different community state types
custom_rel <- transform_abundance(custom_data, type = "relative")
plot_taxa_composition(custom_rel, rank = "Genus", top_n = 8, 
                      group_var = "community_state_type")
```

### Using Your Own Data

If you have your own data, you can import it from common formats:

```r
library(microFGT)

# Import data from dada2
seqtab <- readRDS("seqtab.rds")
taxa <- readRDS("taxa.rds")
metadata <- read.csv("metadata.csv", row.names = 1)

# Create FGTExperiment object
fgt_exp <- import_dada2(seqtab, taxa, metadata)

# Filter and transform data
fgt_exp <- fgt_exp %>%
  filter_taxa(min_prevalence = 0.1, min_abundance = 0.001) %>%
  transform_abundance(type = "relative")

# Plot taxonomic composition
plot_taxa_composition(fgt_exp, rank = "Genus", top_n = 10, 
                     group_var = "subject_group")
```

## Documentation

For detailed documentation and tutorials, please see the package vignettes:

```r
browseVignettes("microFGT")
```

## Pipeline Overview

<img src="man/figures/microFGT_workflow.png" alt="microFGT Workflow" width="700"/>

## Contributing

Contributions to microFGT are welcome! Please see our [Contributing Guidelines](CONTRIBUTING.md) for more details on how to submit issues, feature requests, and pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use microFGT in your research, please cite as:

```
Handley, S. (2023). microFGT: Comprehensive Analysis of Female Genital Tract Microbiome Data. GitHub repository, https://github.com/shandley/microFGT
```

## Acknowledgments

- The microFGT package builds upon the excellent [TreeSummarizedExperiment](https://bioconductor.org/packages/release/bioc/html/TreeSummarizedExperiment.html) package from Bioconductor
- We thank the community of researchers in the field of female genital tract microbiome research for their feedback and suggestions