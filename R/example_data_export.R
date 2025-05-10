#' Export Example Data to Common Bioinformatics Formats
#'
#' This function exports microFGT example data to formats compatible with
#' common bioinformatics tools like phyloseq, DADA2, or QIIME2.
#'
#' @param data Example data (list or FGTExperiment object)
#' @param output_dir Directory to save the exported files
#' @param format Format to export to ("phyloseq", "dada2", "qiime2")
#' @param ... Additional parameters passed to format-specific export functions
#'
#' @return Invisibly returns a list of the exported file paths
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' example_data <- generate_fgt_example_data()
#'
#' # Export to DADA2 format
#' export_example_data(example_data, tempdir(), format = "dada2")
#'
#' # Export to QIIME2 format
#' export_example_data(example_data, tempdir(), format = "qiime2")
#' }
#' @export
export_example_data <- function(data, output_dir, format = c("phyloseq", "dada2", "qiime2"), ...) {
  format <- match.arg(format)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract components from FGTExperiment if needed
  if (methods::is(data, "FGTExperiment")) {
    counts <- SummarizedExperiment::assays(data)$counts
    taxonomy <- SummarizedExperiment::rowData(data)
    metadata <- SummarizedExperiment::colData(data)
    tree <- NULL
    if (methods::is(data, "TreeSummarizedExperiment")) {
      tree <- TreeSummarizedExperiment::rowTree(data)
    }
  } else if (is.list(data)) {
    counts <- data$counts
    taxonomy <- data$taxonomy
    metadata <- data$metadata
    tree <- data$tree
  } else {
    stop("data must be an FGTExperiment object or a list with counts, taxonomy, and metadata components")
  }
  
  # Export to the requested format
  if (format == "phyloseq") {
    return(export_to_phyloseq(counts, taxonomy, metadata, tree, output_dir, ...))
  } else if (format == "dada2") {
    return(export_to_dada2(counts, taxonomy, metadata, output_dir, ...))
  } else if (format == "qiime2") {
    return(export_to_qiime2(counts, taxonomy, metadata, tree, output_dir, ...))
  }
}

#' Export Example Data to phyloseq Format
#'
#' @param counts Count matrix
#' @param taxonomy Taxonomy table
#' @param metadata Sample metadata
#' @param tree Phylogenetic tree
#' @param output_dir Directory to save the exported files
#' @param base_name Base name for the output files
#'
#' @return Invisibly returns a list of the exported file paths
#'
#' @keywords internal
export_to_phyloseq <- function(counts, taxonomy, metadata, tree = NULL, 
                             output_dir, base_name = "phyloseq") {
  # Check if phyloseq package is available
  if (!requireNamespace("phyloseq", quietly = TRUE)) {
    stop("Package 'phyloseq' is required for this export format")
  }
  
  # Create phyloseq object
  otu_table <- phyloseq::otu_table(counts, taxa_are_rows = TRUE)
  tax_table <- phyloseq::tax_table(as.matrix(taxonomy))
  sample_data <- phyloseq::sample_data(metadata)
  
  # Create complete phyloseq object
  if (!is.null(tree) && requireNamespace("ape", quietly = TRUE)) {
    phy_tree <- phyloseq::phy_tree(tree)
    ps <- phyloseq::phyloseq(otu_table, tax_table, sample_data, phy_tree)
  } else {
    ps <- phyloseq::phyloseq(otu_table, tax_table, sample_data)
  }
  
  # Save to RDS file
  file_path <- file.path(output_dir, paste0(base_name, ".rds"))
  saveRDS(ps, file_path)
  
  # Return file path
  invisible(list(phyloseq = file_path))
}

#' Export Example Data to DADA2 Format
#'
#' @param counts Count matrix
#' @param taxonomy Taxonomy table
#' @param metadata Sample metadata
#' @param output_dir Directory to save the exported files
#' @param base_name Base name for the output files
#'
#' @return Invisibly returns a list of the exported file paths
#'
#' @keywords internal
export_to_dada2 <- function(counts, taxonomy, metadata, output_dir, 
                          base_name = "dada2") {
  # Convert taxonomy to DADA2 format if needed
  tax_cols <- colnames(taxonomy)
  
  # Check if taxonomy has the expected columns
  expected_cols <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  if (!all(expected_cols %in% tax_cols)) {
    # Create a matrix with the expected format
    tax_matrix <- matrix(NA, nrow = nrow(taxonomy), ncol = length(expected_cols))
    colnames(tax_matrix) <- expected_cols
    rownames(tax_matrix) <- rownames(taxonomy)
    
    # Fill in the available levels
    for (col in intersect(tax_cols, expected_cols)) {
      tax_matrix[, col] <- taxonomy[, col]
    }
    
    # Convert to DADA2 format (semicolon-separated)
    tax_strings <- apply(tax_matrix, 1, function(row) {
      paste(sapply(seq_along(row), function(i) {
        level <- expected_cols[i]
        value <- row[i]
        if (is.na(value)) {
          paste0(level, ":NA")
        } else {
          paste0(level, ":", value)
        }
      }), collapse = ";")
    })
    
    tax_dada2 <- matrix(tax_strings, ncol = 1)
    colnames(tax_dada2) <- "Taxonomy"
    rownames(tax_dada2) <- rownames(taxonomy)
  } else {
    # Just format as semicolon-separated string
    tax_dada2 <- apply(taxonomy, 1, function(row) {
      paste(sapply(seq_along(row), function(i) {
        paste0(colnames(taxonomy)[i], ":", row[i])
      }), collapse = ";")
    })
    tax_dada2 <- matrix(tax_dada2, ncol = 1)
    colnames(tax_dada2) <- "Taxonomy"
    rownames(tax_dada2) <- rownames(taxonomy)
  }
  
  # Save files
  seqtab_file <- file.path(output_dir, paste0(base_name, "_seqtab.rds"))
  taxa_file <- file.path(output_dir, paste0(base_name, "_taxa.rds"))
  metadata_file <- file.path(output_dir, paste0(base_name, "_metadata.csv"))
  
  saveRDS(counts, seqtab_file)
  saveRDS(tax_dada2, taxa_file)
  utils::write.csv(metadata, metadata_file, row.names = TRUE)
  
  # Return file paths
  invisible(list(
    seqtab = seqtab_file,
    taxa = taxa_file,
    metadata = metadata_file
  ))
}

#' Export Example Data to QIIME2 Format
#'
#' @param counts Count matrix
#' @param taxonomy Taxonomy table
#' @param metadata Sample metadata
#' @param tree Phylogenetic tree
#' @param output_dir Directory to save the exported files
#' @param base_name Base name for the output files
#'
#' @return Invisibly returns a list of the exported file paths
#'
#' @keywords internal
export_to_qiime2 <- function(counts, taxonomy, metadata, tree = NULL, 
                           output_dir, base_name = "qiime2") {
  # Prepare feature table (biom format)
  feature_table_file <- file.path(output_dir, paste0(base_name, "_feature-table.tsv"))
  
  # Write feature table
  feature_table <- data.frame(
    "#OTU ID" = rownames(counts),
    counts,
    check.names = FALSE
  )
  utils::write.table(feature_table, file = feature_table_file, 
                   sep = "\t", row.names = FALSE, quote = FALSE)
  
  # Prepare taxonomy file
  taxonomy_file <- file.path(output_dir, paste0(base_name, "_taxonomy.tsv"))
  
  # Format taxonomy for QIIME2
  tax_strings <- apply(taxonomy, 1, function(row) {
    paste(sapply(seq_along(row), function(i) {
      paste0(colnames(taxonomy)[i], "__", row[i])
    }), collapse = "; ")
  })
  
  tax_qiime <- data.frame(
    "Feature ID" = rownames(taxonomy),
    "Taxon" = tax_strings,
    "Confidence" = 1.0,
    check.names = FALSE
  )
  
  utils::write.table(tax_qiime, file = taxonomy_file, 
                   sep = "\t", row.names = FALSE, quote = FALSE)
  
  # Prepare metadata file
  metadata_file <- file.path(output_dir, paste0(base_name, "_metadata.tsv"))
  
  # Format metadata for QIIME2
  metadata_qiime <- data.frame(
    "#SampleID" = rownames(metadata),
    metadata,
    check.names = FALSE
  )
  
  utils::write.table(metadata_qiime, file = metadata_file, 
                   sep = "\t", row.names = FALSE, quote = FALSE)
  
  # Prepare tree file if provided
  tree_file <- NULL
  if (!is.null(tree) && requireNamespace("ape", quietly = TRUE)) {
    tree_file <- file.path(output_dir, paste0(base_name, "_tree.nwk"))
    ape::write.tree(tree, file = tree_file)
  }
  
  # Return file paths
  result <- list(
    feature_table = feature_table_file,
    taxonomy = taxonomy_file,
    metadata = metadata_file
  )
  
  if (!is.null(tree_file)) {
    result$tree <- tree_file
  }
  
  invisible(result)
}