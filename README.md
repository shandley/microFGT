# microFGT: Comprehensive Analysis of Female Genital Tract Microbiome Data

<img src="https://github.com/shandley/microFGT/blob/main/microFGT_logo.png" alt="microFGT Logo" width="150" align="right"/>

## Overview

microFGT is an integrated R package for comprehensive analysis of female genital tract (FGT) microbiome data. This package unifies specialized tools (dada2, phyloseq, speciateIT, VALENCIA, and VIRGO) within a cohesive framework that handles both amplicon and metagenomic sequencing data.

## Features

- Built on TreeSummarizedExperiment data structure
- Integrates with FGT-specific analysis tools
- Supports both amplicon and metagenomic data
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

## Contributing

Contributions to microFGT are welcome! Please feel free to submit a pull request or open an issue on GitHub.

## License

This project is licensed under the MIT License - see the LICENSE file for details.