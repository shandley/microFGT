# FGT Microbiome Package Implementation Guide

This guide provides practical implementation details for building the FGT microbiome analysis package, focusing on key technical approaches and code patterns.

## Setting Up the Package Structure

### Initial Setup with usethis

```R
# Create package structure
usethis::create_package("fgtMicrobiome")
usethis::use_roxygen_md()
usethis::use_pipe()
usethis::use_rcpp()

# Set up documentation and testing
usethis::use_package_doc()
usethis::use_testthat()
usethis::use_test("core")
usethis::use_vignette("introduction")

# Set up GitHub repository 
usethis::use_git()
usethis::use_github()
```

### Dependencies Configuration

```R
# Add Bioconductor dependencies
usethis::use_bioc_badge()
usethis::use_dev_package("TreeSummarizedExperiment", type = "Imports")
usethis::use_dev_package("SummarizedExperiment", type = "Imports")
usethis::use_dev_package("MultiAssayExperiment", type = "Imports")

# Add tidyverse dependencies
usethis::use_package("dplyr", type = "Imports")
usethis::use_package("tibble", type = "Imports")
usethis::use_package("magrittr", type = "Imports")
usethis::use_package("ggplot2", type = "Imports")

# Add external tool dependencies
usethis::use_package("reticulate", type = "Imports")
```

## Core Data Structures Implementation

### 1. TreeSE-based Container Extension

```R
#' @title FGT Microbiome Experiment class
#' @description S4 class extending TreeSummarizedExperiment for FGT microbiome data
#'
#' @importClassesFrom TreeSummarizedExperiment TreeSummarizedExperiment
#' @importFrom S4Vectors SimpleList
#'
#' @export
setClass("FGTExperiment",
         contains = "TreeSummarizedExperiment",
         slots = list(
           experimentType = "character",
           fgtMetadata = "SimpleList"
         ),
         prototype = list(
           experimentType = "amplicon",
           fgtMetadata = S4Vectors::SimpleList()
         )
)

#' Constructor for FGTExperiment objects
#'
#' @param assays List of matrices or similar objects
#' @param rowData DataFrame of feature metadata
#' @param colData DataFrame of sample metadata
#' @param rowTree phylo object or NULL
#' @param experimentType Type of experiment (amplicon, metagenomic, integrated)
#' @param ... Additional arguments passed to TreeSummarizedExperiment constructor
#'
#' @return An FGTExperiment object
#' @export
FGTExperiment <- function(assays, rowData = NULL, colData = NULL, rowTree = NULL,
                         experimentType = "amplicon", ...) {
  
  # Create TreeSummarizedExperiment
  tse <- TreeSummarizedExperiment::TreeSummarizedExperiment(
    assays = assays,
    rowData = rowData,
    colData = colData,
    rowTree = rowTree,
    ...
  )
  
  # Add FGTExperiment-specific slots
  obj <- new("FGTExperiment", 
            tse,
            experimentType = experimentType,
            fgtMetadata = S4Vectors::SimpleList())
  
  return(obj)
}
```

### 2. Conversion Functions from Phyloseq

```R
#' Convert phyloseq to FGTExperiment
#'
#' @param physeq A phyloseq object
#' @param assay_name Name for the count assay
#'
#' @return An FGTExperiment object
#' @export
#'
#' @importFrom methods is
#' @importFrom SummarizedExperiment assays assays<- rowData rowData<- colData colData<-
#'
phyloseq_to_fgt <- function(physeq, assay_name = "counts") {
  if (!methods::is(physeq, "phyloseq")) {
    stop("Input must be a phyloseq object")
  }
  
  # Extract components from phyloseq
  otu_table <- phyloseq::otu_table(physeq)
  tax_table <- phyloseq::tax_table(physeq)
  sample_data <- phyloseq::sample_data(physeq)
  phy_tree <- phyloseq::phy_tree(physeq)
  
  # Ensure OTU table is taxa x samples
  if (phyloseq::taxa_are_rows(otu_table) == FALSE) {
    otu_table <- t(otu_table)
  }
  
  # Create assays list
  assays_list <- list()
  assays_list[[assay_name]] <- as.matrix(otu_table)
  
  # Convert taxonomy table to rowData
  if (!is.null(tax_table)) {
    rowData_df <- as.data.frame(tax_table)
  } else {
    rowData_df <- data.frame(row.names = rownames(otu_table))
  }
  
  # Convert sample_data to colData
  if (!is.null(sample_data)) {
    colData_df <- as.data.frame(sample_data)
  } else {
    colData_df <- data.frame(row.names = colnames(otu_table))
  }
  
  # Create FGTExperiment
  fgt_exp <- FGTExperiment(
    assays = assays_list,
    rowData = rowData_df,
    colData = colData_df,
    rowTree = phy_tree,
    experimentType = "amplicon"
  )
  
  return(fgt_exp)
}
```

### 3. Data Import from dada2

```R
#' Import dada2 results into FGTExperiment
#'
#' @param seqtab Sequence table from dada2
#' @param taxa Taxonomy table from dada2
#' @param sample_data Sample metadata data frame
#' @param refseq Optional DNAStringSet of reference sequences
#'
#' @return An FGTExperiment object
#' @export
#'
#' @importFrom Biostrings DNAStringSet
#'
import_dada2 <- function(seqtab, taxa = NULL, sample_data = NULL, refseq = NULL) {
  # Ensure seqtab is a matrix
  if (!is.matrix(seqtab)) {
    seqtab <- as.matrix(seqtab)
  }
  
  # Create sequence names if they don't exist
  if (is.null(rownames(seqtab))) {
    if (is.null(colnames(seqtab))) {
      stop("Sequence table must have row or column names")
    }
    seqtab <- t(seqtab)  # Transpose to make sequences as rows
  }
  
  # Process taxonomy table
  if (!is.null(taxa)) {
    # Convert to data frame if matrix
    if (is.matrix(taxa)) {
      taxa <- as.data.frame(taxa)
    }
    
    # Ensure sequence IDs match
    if (!all(rownames(seqtab) %in% rownames(taxa))) {
      warning("Not all sequences in seqtab have taxonomy assignments")
      # Subset to matching sequences
      shared_seqs <- intersect(rownames(seqtab), rownames(taxa))
      seqtab <- seqtab[shared_seqs, , drop = FALSE]
      taxa <- taxa[shared_seqs, , drop = FALSE]
    }
    
    rowData_df <- taxa
  } else {
    # Create empty rowData
    rowData_df <- data.frame(row.names = rownames(seqtab))
  }
  
  # Add sequences to rowData
  rowData_df$sequence <- rownames(seqtab)
  
  # Process sample metadata
  if (!is.null(sample_data)) {
    if (!all(colnames(seqtab) %in% rownames(sample_data))) {
      warning("Not all samples in seqtab have metadata")
      # Subset to matching samples
      shared_samples <- intersect(colnames(seqtab), rownames(sample_data))
      seqtab <- seqtab[, shared_samples, drop = FALSE]
    }
    colData_df <- sample_data[colnames(seqtab), , drop = FALSE]
  } else {
    # Create empty colData
    colData_df <- data.frame(row.names = colnames(seqtab))
  }
  
  # Create reference sequence object if provided
  if (!is.null(refseq)) {
    if (!methods::is(refseq, "DNAStringSet")) {
      refseq <- Biostrings::DNAStringSet(refseq)
      names(refseq) <- rownames(seqtab)
    }
  } else {
    # Create DNAStringSet from sequence IDs
    refseq <- Biostrings::DNAStringSet(rownames(seqtab))
    names(refseq) <- rownames(seqtab)
  }
  
  # Create FGTExperiment
  fgt_exp <- FGTExperiment(
    assays = list(counts = seqtab),
    rowData = rowData_df,
    colData = colData_df,
    referenceSeq = refseq,
    experimentType = "amplicon"
  )
  
  return(fgt_exp)
}
```

## Tidyverse-Compatible Functions Implementation

### 1. Method Chain Design Pattern

```R
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Filter taxa based on prevalence and abundance
#'
#' @param fgt_exp An FGTExperiment object
#' @param min_prevalence Minimum prevalence (proportion of samples)
#' @param min_abundance Minimum relative abundance
#' @param assay_name Name of the count assay to use
#'
#' @return Filtered FGTExperiment object
#' @export
filter_taxa <- function(fgt_exp, min_prevalence = 0.1, min_abundance = 0.001, 
                        assay_name = "counts") {
  # Input validation
  if (!methods::is(fgt_exp, "FGTExperiment")) {
    stop("Input must be an FGTExperiment object")
  }
  
  # Extract count matrix
  counts <- SummarizedExperiment::assays(fgt_exp)[[assay_name]]
  
  # Calculate prevalence and abundance
  prevalence <- rowSums(counts > 0) / ncol(counts)
  rel_abundance <- rowSums(counts) / sum(counts)
  
  # Apply filters
  keep_taxa <- prevalence >= min_prevalence & rel_abundance >= min_abundance
  
  # Subset the object
  fgt_exp_filtered <- fgt_exp[keep_taxa, ]
  
  return(fgt_exp_filtered)
}

#' Transform counts to different abundance metrics
#'
#' @param fgt_exp An FGTExperiment object
#' @param type Type of transformation ("relative", "clr", "log", "presence")
#' @param assay_name Name of the count assay to use
#' @param new_assay_name Name for the new assay
#' @param pseudocount Pseudocount to add for log and clr transformations
#'
#' @return FGTExperiment with additional assay
#' @export
transform_abundance <- function(fgt_exp, type = "relative", assay_name = "counts",
                              new_assay_name = NULL, pseudocount = 1) {
  # Input validation
  if (!methods::is(fgt_exp, "FGTExperiment")) {
    stop("Input must be an FGTExperiment object")
  }
  
  # Extract count matrix
  counts <- SummarizedExperiment::assays(fgt_exp)[[assay_name]]
  
  # Set new assay name if not provided
  if (is.null(new_assay_name)) {
    new_assay_name <- type
  }
  
  # Apply transformation
  transformed <- switch(
    type,
    "relative" = counts / rowSums(counts),
    "clr" = log(counts + pseudocount) - rowMeans(log(counts + pseudocount)),
    "log" = log(counts + pseudocount),
    "presence" = counts > 0,
    stop("Unsupported transformation type")
  )
  
  # Add new assay to object
  SummarizedExperiment::assays(fgt_exp)[[new_assay_name]] <- transformed
  
  return(fgt_exp)
}
```

### 2. Accessor Functions with Tibble Output

```R
#' Extract taxa abundance data as tibble
#'
#' @param fgt_exp An FGTExperiment object
#' @param assay_name Name of the assay to extract
#' @param include_metadata Include taxa metadata columns
#'
#' @return A tibble with abundance data
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
get_taxa_abundances <- function(fgt_exp, assay_name = "counts", include_metadata = TRUE) {
  # Extract abundance data
  abund_mat <- SummarizedExperiment::assays(fgt_exp)[[assay_name]]
  
  # Convert to tibble
  abund_tib <- tibble::as_tibble(abund_mat, rownames = "taxon_id")
  
  # Add metadata if requested
  if (include_metadata) {
    taxa_meta <- tibble::as_tibble(SummarizedExperiment::rowData(fgt_exp), 
                                  rownames = "taxon_id")
    
    # Ensure taxon_id columns match
    if (!identical(abund_tib$taxon_id, taxa_meta$taxon_id)) {
      taxa_meta <- taxa_meta[match(abund_tib$taxon_id, taxa_meta$taxon_id), ]
    }
    
    # Drop duplicate taxon_id column
    taxa_meta$taxon_id <- NULL
    
    # Combine abundance and metadata
    result <- dplyr::bind_cols(abund_tib, taxa_meta)
  } else {
    result <- abund_tib
  }
  
  return(result)
}

#' Extract sample data as tibble
#'
#' @param fgt_exp An FGTExperiment object
#'
#' @return A tibble with sample metadata
#' @export
#'
#' @importFrom tibble as_tibble
get_sample_data <- function(fgt_exp) {
  sample_df <- SummarizedExperiment::colData(fgt_exp)
  tibble::as_tibble(sample_df, rownames = "sample_id")
}
```

## speciateIT Integration Example

### 1. System Call Wrapper for speciateIT

```R
#' Run speciateIT taxonomic classification
#'
#' @param fasta_file Path to FASTA file with sequences
#' @param output_file Path to output file (will be created)
#' @param reference_db Path to reference database
#' @param threads Number of threads to use
#' @param other_params Additional parameters for speciateIT
#'
#' @return Path to output file
#' @export
run_speciateit <- function(fasta_file, output_file = NULL, 
                          reference_db = default_speciateit_db(),
                          threads = 1, other_params = "") {
  
  # Check if speciateIT is installed
  if (!is_speciateit_available()) {
    stop("speciateIT is not installed. Use install_speciateit() to install it.")
  }
  
  # Create output file name if not provided
  if (is.null(output_file)) {
    output_file <- paste0(tools::file_path_sans_ext(fasta_file), "_speciateit.tsv")
  }
  
  # Build command
  cmd <- paste0(
    "speciateit",
    " --input ", shQuote(fasta_file),
    " --output ", shQuote(output_file),
    " --refdb ", shQuote(reference_db),
    " --threads ", threads,
    " ", other_params
  )
  
  # Run command
  system_result <- system(cmd, intern = TRUE)
  
  # Check if output file was created
  if (!file.exists(output_file)) {
    stop("speciateIT failed to create output file")
  }
  
  return(output_file)
}
```

### 2. Integration with FGTExperiment

```R
#' Apply speciateIT classification to FGTExperiment
#'
#' @param fgt_exp An FGTExperiment object
#' @param sequence_col Column name in rowData containing sequences
#' @param reference_db Path to reference database
#' @param threads Number of threads
#'
#' @return FGTExperiment with updated taxonomy
#' @export
classify_with_speciateit <- function(fgt_exp, sequence_col = "sequence",
                                    reference_db = default_speciateit_db(),
                                    threads = 1) {
  # Extract sequences
  row_data <- SummarizedExperiment::rowData(fgt_exp)
  sequences <- row_data[[sequence_col]]
  
  if (is.null(sequences)) {
    stop("Sequence column not found in rowData")
  }
  
  # Create temporary FASTA file
  temp_fasta <- tempfile(fileext = ".fasta")
  temp_out <- tempfile(fileext = ".tsv")
  
  # Write sequences to FASTA
  writeLines(
    c(mapply(function(id, seq) c(paste0(">", id), seq), 
            rownames(row_data), sequences)),
    temp_fasta
  )
  
  # Run speciateIT
  speciateit_out <- run_speciateit(
    fasta_file = temp_fasta,
    output_file = temp_out,
    reference_db = reference_db,
    threads = threads
  )
  
  # Read results
  tax_results <- read.delim(speciateit_out, stringsAsFactors = FALSE)
  
  # Match results with sequences in FGTExperiment
  tax_results <- tax_results[match(rownames(row_data), tax_results$sequence_id), ]
  
  # Update rowData with taxonomic information
  tax_cols <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  for (col in tax_cols) {
    if (col %in% colnames(tax_results)) {
      row_data[[col]] <- tax_results[[col]]
    }
  }
  
  # Add confidence scores if available
  conf_cols <- grep("confidence", colnames(tax_results), value = TRUE)
  for (col in conf_cols) {
    row_data[[col]] <- tax_results[[col]]
  }
  
  # Update the FGTExperiment
  SummarizedExperiment::rowData(fgt_exp) <- row_data
  
  # Clean up temporary files
  unlink(c(temp_fasta, temp_out))
  
  return(fgt_exp)
}
```

## Testing Framework

### 1. Unit Tests for Core Functions

```R
# tests/testthat/test-core.R

test_that("FGTExperiment constructor works", {
  # Create simple test data
  counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10)
  rownames(counts) <- paste0("OTU", 1:10)
  colnames(counts) <- paste0("Sample", 1:10)
  
  # Create minimal FGTExperiment
  fgt_exp <- FGTExperiment(assays = list(counts = counts))
  
  # Tests
  expect_s4_class(fgt_exp, "FGTExperiment")
  expect_s4_class(fgt_exp, "TreeSummarizedExperiment")
  expect_equal(dim(fgt_exp), c(10, 10))
  expect_equal(FGTExperiment::experimentType(fgt_exp), "amplicon")
})

test_that("filter_taxa works correctly", {
  # Create test data
  counts <- matrix(c(
    # High prevalence, high abundance
    rep(10, 10),
    # Low prevalence, high abundance 
    c(100, rep(0, 9)),
    # High prevalence, low abundance
    rep(1, 10)
  ), nrow = 3, byrow = TRUE)
  
  rownames(counts) <- c("high_both", "low_prev", "low_abund")
  colnames(counts) <- paste0("S", 1:10)
  
  fgt_exp <- FGTExperiment(assays = list(counts = counts))
  
  # Filter with different thresholds
  res1 <- filter_taxa(fgt_exp, min_prevalence = 0.5, min_abundance = 0.01)
  res2 <- filter_taxa(fgt_exp, min_prevalence = 0.5, min_abundance = 0.2)
  res3 <- filter_taxa(fgt_exp, min_prevalence = 0.2, min_abundance = 0.01)
  
  # Tests
  expect_equal(dim(res1), c(2, 10))  # Both high_both and low_abund pass
  expect_equal(dim(res2), c(1, 10))  # Only high_both passes
  expect_equal(dim(res3), c(3, 10))  # All taxa pass
})

test_that("transform_abundance works correctly", {
  # Create test data
  counts <- matrix(1:12, nrow = 3, ncol = 4)
  rownames(counts) <- paste0("OTU", 1:3)
  colnames(counts) <- paste0("S", 1:4)
  
  fgt_exp <- FGTExperiment(assays = list(counts = counts))
  
  # Apply transformations
  rel_result <- transform_abundance(fgt_exp, type = "relative")
  log_result <- transform_abundance(fgt_exp, type = "log")
  clr_result <- transform_abundance(fgt_exp, type = "clr")
  
  # Tests
  expect_true("relative" %in% names(SummarizedExperiment::assays(rel_result)))
  expect_true("log" %in% names(SummarizedExperiment::assays(log_result)))
  expect_true("clr" %in% names(SummarizedExperiment::assays(clr_result)))
  
  # Check relative abundance sums to 1
  expect_equal(
    colSums(SummarizedExperiment::assays(rel_result)[["relative"]]),
    rep(1, 4)
  )
})
```

### 2. Integration Tests

```R
# tests/testthat/test-integration.R

test_that("Full workflow from dada2 to community typing", {
  skip_if_not_installed("dada2")
  skip_if_not_installed("DECIPHER")
  
  # Load example data (this would come from dada2 in practice)
  data(example_seqtab, package = "fgtMicrobiome")
  data(example_taxa, package = "fgtMicrobiome")
  data(example_metadata, package = "fgtMicrobiome")
  
  # Create FGTExperiment
  fgt_exp <- import_dada2(
    seqtab = example_seqtab,
    taxa = example_taxa,
    sample_data = example_metadata
  )
  
  # Process data
  fgt_exp <- fgt_exp %>%
    filter_taxa(min_prevalence = 0.1) %>%
    transform_abundance(type = "relative")
  
  # Run community typing
  fgt_exp <- assign_cst(fgt_exp)
  
  # Verify results
  expect_true("CST" %in% colnames(SummarizedExperiment::colData(fgt_exp)))
  expect_true(all(SummarizedExperiment::colData(fgt_exp)$CST %in% 
                  c("I", "II", "III", "IV", "V", "NA")))
})
```

## Performance Optimizations

### 1. Rcpp Implementation for Count Transformations

```cpp
// src/count_transforms.cpp

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cpp_rel_abundance(NumericMatrix counts) {
  int nrow = counts.nrow();
  int ncol = counts.ncol();
  
  NumericMatrix result(nrow, ncol);
  
  for (int j = 0; j < ncol; j++) {
    double col_sum = 0;
    
    // Calculate column sum
    for (int i = 0; i < nrow; i++) {
      col_sum += counts(i, j);
    }
    
    // Calculate relative abundance
    if (col_sum > 0) {
      for (int i = 0; i < nrow; i++) {
        result(i, j) = counts(i, j) / col_sum;
      }
    }
  }
  
  // Set row and column names
  result.attr("dimnames") = counts.attr("dimnames");
  
  return result;
}

// [[Rcpp::export]]
NumericMatrix cpp_clr_transform(NumericMatrix counts, double pseudocount = 1.0) {
  int nrow = counts.nrow();
  int ncol = counts.ncol();
  
  NumericMatrix result(nrow, ncol);
  
  for (int j = 0; j < ncol; j++) {
    NumericVector log_counts(nrow);
    double geom_mean = 0;
    
    // Calculate log counts and geometric mean
    for (int i = 0; i < nrow; i++) {
      log_counts[i] = log(counts(i, j) + pseudocount);
      geom_mean += log_counts[i];
    }
    geom_mean /= nrow;
    
    // Calculate CLR transform
    for (int i = 0; i < nrow; i++) {
      result(i, j) = log_counts[i] - geom_mean;
    }
  }
  
  // Set row and column names
  result.attr("dimnames") = counts.attr("dimnames");
  
  return result;
}
```

### 2. R Interface to Rcpp Functions

```R
#' Fast relative abundance calculation using C++
#'
#' @param counts A count matrix
#'
#' @return Relative abundance matrix
#' @keywords internal
rel_abundance_fast <- function(counts) {
  # Ensure numeric matrix
  counts <- as.matrix(counts)
  storage.mode(counts) <- "numeric"
  
  # Call C++ function
  result <- cpp_rel_abundance(counts)
  
  return(result)
}

#' Fast CLR transformation using C++
#'
#' @param counts A count matrix
#' @param pseudocount Pseudocount to add before log transformation
#'
#' @return CLR transformed matrix
#' @keywords internal
clr_transform_fast <- function(counts, pseudocount = 1.0) {
  # Ensure numeric matrix
  counts <- as.matrix(counts)
  storage.mode(counts) <- "numeric"
  
  # Call C++ function
  result <- cpp_clr_transform(counts, pseudocount)
  
  return(result)
}

# Update transform_abundance function to use optimized versions
transform_abundance <- function(fgt_exp, type = "relative", assay_name = "counts",
                              new_assay_name = NULL, pseudocount = 1) {
  # Input validation
  if (!methods::is(fgt_exp, "FGTExperiment")) {
    stop("Input must be an FGTExperiment object")
  }
  
  # Extract count matrix
  counts <- SummarizedExperiment::assays(fgt_exp)[[assay_name]]
  
  # Set new assay name if not provided
  if (is.null(new_assay_name)) {
    new_assay_name <- type
  }
  
  # Apply transformation using optimized functions
  transformed <- switch(
    type,
    "relative" = rel_abundance_fast(counts),
    "clr" = clr_transform_fast(counts, pseudocount),
    "log" = log(counts + pseudocount),
    "presence" = counts > 0,
    stop("Unsupported transformation type")
  )
  
  # Add new assay to object
  SummarizedExperiment::assays(fgt_exp)[[new_assay_name]] <- transformed
  
  return(fgt_exp)
}
```

## Handling Large Datasets

```R
#' Create on-disk backed FGTExperiment
#'
#' @param fgt_exp An in-memory FGTExperiment
#' @param chunk_size Chunk size for HDF5 storage
#' @param dir Directory to store HDF5 file
#'
#' @return FGTExperiment with HDF5-backed assays
#' @export
to_disk_backed <- function(fgt_exp, chunk_size = NULL, dir = tempdir()) {
  if (!requireNamespace("HDF5Array", quietly = TRUE)) {
    stop("Package 'HDF5Array' is required for disk-backed storage")
  }
  
  # Create unique filename
  h5_file <- file.path(dir, paste0("fgt_exp_", digest::digest(Sys.time(), algo = "md5"), ".h5"))
  
  # Get assay names
  assay_names <- names(SummarizedExperiment::assays(fgt_exp))
  
  # Convert each assay to HDF5-backed array
  for (assay_name in assay_names) {
    current_assay <- SummarizedExperiment::assays(fgt_exp)[[assay_name]]
    
    # Convert to HDF5Array
    if (is.null(chunk_size)) {
      h5_assay <- HDF5Array::writeHDF5Array(current_assay, filepath = h5_file, name = assay_name)
    } else {
      h5_assay <- HDF5Array::writeHDF5Array(current_assay, filepath = h5_file, name = assay_name,
                                         chunk = chunk_size)
    }
    
    # Replace in-memory assay with HDF5-backed one
    SummarizedExperiment::assays(fgt_exp)[[assay_name]] <- h5_assay
  }
  
  # Add file path to metadata
  S4Vectors::metadata(fgt_exp)$h5_file <- h5_file
  
  return(fgt_exp)
}
```

## Documentation Example

```R
#' @title FGT Microbiome Analysis Package
#' @description 
#' A unified framework for analyzing microbiome data from the female genital tract.
#' This package integrates amplicon and metagenomic data analysis tools including
#' dada2, phyloseq, speciateIT, VALENCIA, and VIRGO into a cohesive workflow.
#'
#' @section Core data structures:
#' The package uses TreeSummarizedExperiment as its foundation, extending it with
#' specialized methods for FGT microbiome analysis.
#'
#' @section Key components:
#' \itemize{
#'   \item Data import from common formats (dada2, phyloseq)
#'   \item Taxonomic analysis with speciateIT integration
#'   \item Community state typing with VALENCIA
#'   \item Metagenomic analysis with VIRGO
#'   \item Multi-omics integration
#' }
#'
#' @docType package
#' @name fgtMicrobiome-package
#' @aliases fgtMicrobiome
NULL
```
