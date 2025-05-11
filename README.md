# microFGT: Comprehensive Analysis of Female Genital Tract Microbiome Data

<img src="inst/extdata/microFGT_logo.png" alt="microFGT Logo" width="150" align="right"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/shandley/microFGT/workflows/R-CMD-check/badge.svg)](https://github.com/shandley/microFGT/actions)
[![Codecov test coverage](https://codecov.io/gh/shandley/microFGT/branch/main/graph/badge.svg)](https://codecov.io/gh/shandley/microFGT?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

microFGT is an integrated R package for comprehensive analysis of female genital tract (FGT) microbiome data. This package unifies specialized tools for microbiome analysis within a cohesive framework that handles both amplicon and metagenomic sequencing data.

> 🚧 **ALPHA STAGE SOFTWARE**: This package is in early development and not ready for production use. APIs may change, functions may not work as expected, and documentation may be incomplete.

## Features

- **Hybrid Architecture**:
  - Minimal S4 class layer for Bioconductor compatibility
  - Function-centric design for core functionality
  - S3 methods for visualization
- **Universal Compatibility**:
  - Works with SummarizedExperiment objects
  - Extended functionality with FGTExperiment objects
  - Seamless integration with the Bioconductor ecosystem
- **Comprehensive Analysis Tools**:
  - Taxonomic aggregation and manipulation
  - Diversity calculation and visualization
  - Data transformation and normalization
  - Community composition analysis
- **Beginner-Friendly**:
  - Includes realistic example datasets
  - Clear documentation and examples
  - Progressive complexity for different user levels

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

microFGT includes built-in example data that simulates realistic FGT microbiome profiles:

```r
library(microFGT)

# Load pre-built example dataset
fgt_exp <- load_example_data(size = "small", type = "amplicon")

# View basic information about the dataset
fgt_exp

# See sample metadata
colData(fgt_exp)[, c("condition", "pH", "Nugent_Score")]
```

### Function-Based Analysis

The hybrid approach allows you to analyze data with straightforward functions:

```r
# Start with any SummarizedExperiment-like object
se <- load_example_data()

# Transform data with standalone functions
se <- transform_abundance(se, method = "relative", assay_name = "counts")

# Aggregate at genus level
genus_level <- aggregate_taxa(se, rank = "Genus")

# Calculate diversity
div_results <- calculate_diversity(genus_level, method = "shannon")

# Create a customized plot
plot_composition(genus_level, level = "Genus", top_n = 10)
```

### Working with Taxonomic Data

```r
# Load example data
se <- load_example_data()

# Clean up taxonomy
se <- normalize_taxonomy(se)

# Get available taxonomic ranks
ranks <- get_taxonomic_ranks(se)
print(ranks)

# Aggregate at phylum level
phylum_level <- aggregate_taxa(se, rank = "Phylum")

# Create formatted taxonomy strings
phylum_level <- create_tax_strings(phylum_level, format = "lineage")
```

### Import and Export Data

```r
# Import from various formats
se <- import_microbiome("counts.csv", "taxonomy.csv", "metadata.csv")

# Import from DADA2 results
se <- import_from_dada2("seqtab.rds", "taxa.rds", "metadata.csv")

# Export to QIIME2 format
export_microbiome(se, "output_dir", format = "qiime2")

# Export to phyloseq format
export_microbiome(se, "output_dir", format = "phyloseq")

# Convert to phyloseq object for further analysis
ps <- to_phyloseq(se)
```

### Calculate Diversity

```r
# Calculate alpha diversity
shannon <- calculate_diversity(se, method = "shannon")
simpson <- calculate_diversity(se, method = "simpson")
richness <- calculate_diversity(se, method = "richness")

# Calculate beta diversity
bray_dist <- calculate_beta_diversity(se, method = "bray")
jaccard_dist <- calculate_beta_diversity(se, method = "jaccard")
```

## Documentation

For detailed documentation and tutorials, please see the package vignettes:

```r
browseVignettes("microFGT")
```

## Architecture

microFGT uses a hybrid approach that combines:

1. **Minimal S4 Classes**: A thin layer extending TreeSummarizedExperiment for Bioconductor compatibility
2. **Standalone Functions**: Core functionality implemented as independent functions
3. **S3 Methods**: Visualization and common operations using S3 method dispatch

This design offers several advantages:
- Easier testing and maintenance
- Better interoperability with other packages
- Reduced complexity for users
- Progressive learning curve

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