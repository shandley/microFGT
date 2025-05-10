# FGT Microbiome Package Architecture

## Package Structure
```
fgtMicrobiome/
├── R/
│   ├── data_structures.R       # Core TreeSE implementation and extensions
│   ├── import.R                # Data import functions (dada2, phyloseq, etc.)
│   ├── conversions.R           # Format conversion utilities
│   ├── taxonomic_analysis.R    # Functions for taxonomic operations
│   ├── community_typing.R      # VALENCIA implementation and CST functions
│   ├── metagenomic_analysis.R  # VIRGO integration and functions
│   ├── multi_omics.R           # Integration of amplicon and metagenomic data
│   ├── visualization.R         # Plotting and visualization functions
│   └── utils.R                 # Helper utilities and internal functions
├── src/
│   ├── RcppExports.cpp         # Rcpp exports
│   ├── taxonomic_ops.cpp       # C++ implementations for taxonomic operations
│   └── count_transforms.cpp    # Optimized count transformation functions
├── inst/
│   ├── extdata/                # External data files
│   │   ├── reference_dbs/      # Reference databases
│   │   └── example_data/       # Example datasets
│   └── scripts/                # Helper scripts for external tools
├── man/                        # Documentation
├── vignettes/                  # Package vignettes
│   ├── introduction.Rmd        # Getting started guide
│   ├── amplicon_workflow.Rmd   # Amplicon data analysis
│   ├── metagenomic_workflow.Rmd # Metagenomic data analysis
│   └── integrated_analysis.Rmd # Combined analysis workflow
├── tests/                      # Unit tests
│   └── testthat/
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Exported functions and imports
└── README.md                   # Package overview
```

## Core Data Structures

```R
# TreeSummarizedExperiment extensions for FGT microbiome data
# Stored in data_structures.R

#' Create an FGT microbiome experiment object
#'
#' @param counts A matrix or data frame of count data with taxa as rows and samples as columns
#' @param taxonomy A data frame of taxonomic classifications
#' @param sample_data A data frame of sample metadata
#' @param tree A phylogenetic tree (optional)
#' @param ... Additional parameters
#'
#' @return A TreeSummarizedExperiment object configured for FGT microbiome analysis
#' @export
create_fgt_experiment <- function(counts, taxonomy, sample_data, tree = NULL, ...) {
  # Implementation details
}

#' Convert a phyloseq object to FGT experiment
#'
#' @param physeq A phyloseq object
#'
#' @return A TreeSummarizedExperiment object
#' @export
phyloseq_to_fgt <- function(physeq) {
  # Implementation details
}

#' Create a multi-omic FGT experiment
#'
#' @param amplicon_data An FGT experiment containing amplicon data
#' @param metagenomic_data An FGT experiment containing metagenomic data
#' @param sample_relationships Data frame linking samples across experiments
#'
#' @return A MultiAssayExperiment object
#' @export
create_multiomic_fgt <- function(amplicon_data, metagenomic_data, sample_relationships = NULL) {
  # Implementation details
}
```

## Component Integration Strategy

### speciateIT Integration
```R
# Integration with speciateIT taxonomic classifier
# Stored in taxonomic_analysis.R

#' Run speciateIT classification on sequence data
#'
#' @param sequences Character vector of sequences or path to FASTA file
#' @param reference_db Path to reference database
#' @param threads Number of threads to use
#'
#' @return Data frame of taxonomic classifications
#' @export
run_speciateit <- function(sequences, reference_db = default_speciateit_db(), threads = 1) {
  # Implementation details
}

#' Apply speciateIT classification to an FGT experiment
#'
#' @param fgt_exp An FGT experiment object
#' @param sequence_col Name of column containing sequences
#' @param reference_db Path to reference database
#'
#' @return Updated FGT experiment with taxonomic classifications
#' @export
classify_with_speciateit <- function(fgt_exp, sequence_col = "sequence", 
                                    reference_db = default_speciateit_db()) {
  # Implementation details
}
```

### VALENCIA Integration
```R
# Integration with VALENCIA community state typing
# Stored in community_typing.R

#' Run VALENCIA CST assignment
#'
#' @param fgt_exp An FGT experiment object
#' @param abundance_type Type of abundance to use (default: "relative")
#'
#' @return Updated FGT experiment with CST assignments
#' @export
assign_cst <- function(fgt_exp, abundance_type = "relative") {
  # Implementation details
}

#' Plot community state type composition
#'
#' @param fgt_exp An FGT experiment with CST assignments
#' @param group_var Optional grouping variable for comparison
#'
#' @return ggplot object
#' @export
plot_cst_composition <- function(fgt_exp, group_var = NULL) {
  # Implementation details
}
```

### VIRGO Integration
```R
# Integration with VIRGO metagenomic analysis
# Stored in metagenomic_analysis.R

#' Run VIRGO gene mapping
#'
#' @param reads Path to reads files
#' @param output_dir Directory for output
#' @param threads Number of threads to use
#'
#' @return Path to output directory
#' @export
run_virgo_mapping <- function(reads, output_dir = tempdir(), threads = 1) {
  # Implementation details
}

#' Import VIRGO results into FGT experiment
#'
#' @param virgo_dir Directory containing VIRGO results
#'
#' @return FGT experiment object
#' @export
import_virgo <- function(virgo_dir) {
  # Implementation details
}

#' Assign metagenomic CSTs using mgCST
#'
#' @param fgt_exp An FGT experiment with VIRGO data
#'
#' @return Updated FGT experiment with mgCST assignments
#' @export
assign_mgcst <- function(fgt_exp) {
  # Implementation details
}
```

## Usage Workflow Examples

### Basic Amplicon Workflow
```R
# Example workflow for amplicon data analysis
library(fgtMicrobiome)

# Import data from dada2
seqtab <- readRDS("seqtab.rds")
taxa <- readRDS("taxa.rds")
metadata <- read.csv("metadata.csv")

# Create FGT experiment
fgt_exp <- create_fgt_experiment(counts = seqtab, 
                                taxonomy = taxa,
                                sample_data = metadata)

# Preprocess data
fgt_exp <- fgt_exp %>%
  filter_taxa(min_prevalence = 0.01, min_abundance = 0.001) %>%
  transform_abundances(type = "clr") 

# Run community state typing
fgt_exp <- assign_cst(fgt_exp)

# Visualize results
plot_cst_composition(fgt_exp, group_var = "study_group")
plot_taxa_composition(fgt_exp, rank = "Genus", top_n = 10)
```

### Combined Multi-omic Workflow
```R
# Example workflow for combined amplicon and metagenomic analysis
library(fgtMicrobiome)

# Import amplicon data
amplicon_exp <- import_dada2("dada2_output/")

# Import metagenomic data
metagenomic_exp <- import_virgo("virgo_output/")

# Create multi-omic experiment
multi_exp <- create_multiomic_fgt(amplicon_exp, metagenomic_exp)

# Run integrated analysis
results <- run_correlation_analysis(multi_exp, 
                                   taxa_level = "Species",
                                   func_level = "KO")

# Visualize correlations
plot_omics_correlations(results, top_n = 20)
```

## Dependencies Management

### Key Package Dependencies
```
# From DESCRIPTION file
Imports:
    TreeSummarizedExperiment,
    SummarizedExperiment,
    MultiAssayExperiment,
    SingleCellExperiment,
    BiocGenerics,
    S4Vectors,
    IRanges,
    Biostrings,
    dplyr,
    tibble,
    ggplot2,
    magrittr,
    Rcpp (>= 1.0.5),
    reticulate

Suggests:
    phyloseq,
    dada2,
    DECIPHER,
    rhdf5,
    BiocStyle,
    knitr,
    rmarkdown,
    testthat (>= 3.0.0),
    vegan,
    DESeq2
```

### External Tool Management
```R
# Utility functions for managing external tools
# Stored in utils.R

#' Check if speciateIT is installed
#'
#' @return Logical indicating if speciateIT is installed
#' @export
is_speciateit_available <- function() {
  # Implementation details
}

#' Install speciateIT
#'
#' @param force Force reinstallation if already installed
#'
#' @return Logical indicating success
#' @export
install_speciateit <- function(force = FALSE) {
  # Implementation details
}

#' Download and configure VIRGO reference database
#'
#' @param destination Directory to store the database
#'
#' @return Path to the installed database
#' @export
setup_virgo_database <- function(destination = system.file("extdata", 
                                                         "reference_dbs", 
                                                         package = "fgtMicrobiome")) {
  # Implementation details
}
```
