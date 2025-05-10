#!/usr/bin/env Rscript

# Test microFGT core functionality
library(microFGT)

# Create example data
set.seed(42)

# Create simple count matrix
counts <- matrix(
  rpois(300, lambda = 20), 
  nrow = 30,  # 30 features/taxa
  ncol = 10   # 10 samples
)
rownames(counts) <- paste0("Feature", 1:30)
colnames(counts) <- paste0("Sample", 1:10)

# Create taxonomy table with 7 levels
taxa_levels <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
taxa <- matrix(NA, nrow = 30, ncol = length(taxa_levels))
colnames(taxa) <- taxa_levels

# Fill taxonomy with realistic bacterial names
kingdom <- rep("Bacteria", 30)
phyla <- sample(c("Firmicutes", "Bacteroidetes", "Proteobacteria", "Actinobacteria"), 30, replace = TRUE)
classes <- paste0(phyla, "_class")
orders <- paste0(classes, "_order")
families <- paste0(orders, "_family")
genera <- paste0(families, "_genus")
species <- paste0(genera, "_sp")

taxa[, "Kingdom"] <- kingdom
taxa[, "Phylum"] <- phyla
taxa[, "Class"] <- classes
taxa[, "Order"] <- orders
taxa[, "Family"] <- families
taxa[, "Genus"] <- genera
taxa[, "Species"] <- species
rownames(taxa) <- rownames(counts)

# Create sample metadata
sample_data <- data.frame(
  row.names = colnames(counts),
  Group = rep(c("Control", "Treatment"), each = 5),
  Age = sample(20:50, 10, replace = TRUE),
  pH = runif(10, 3.8, 4.5)
)

# Create a simple tree
if (requireNamespace("ape", quietly = TRUE)) {
  library(ape)
  set.seed(42)
  tree <- rtree(30, rooted = TRUE)
  tree$tip.label <- rownames(counts)
} else {
  tree <- NULL
  message("ape package not available, skipping tree creation")
}

# Convert to FGTExperiment object
tryCatch({
  cat("\n1. Testing core FGTExperiment creation...\n")
  if (exists("tree")) {
    fgt_obj <- FGTExperiment(
      assays = list(counts = counts),
      rowData = taxa,
      colData = sample_data,
      rowTree = tree,
      experimentType = "amplicon"
    )
  } else {
    fgt_obj <- FGTExperiment(
      assays = list(counts = counts),
      rowData = taxa,
      colData = sample_data,
      experimentType = "amplicon"
    )
  }
  
  # Test basic object methods
  cat("   Basic object info:\n")
  print(fgt_obj)
  
  # Test data manipulation functions
  cat("\n2. Testing data manipulation functions...\n")
  
  # Transform abundance
  cat("   Testing transform_abundance()...\n")
  fgt_rel <- transform_abundance(fgt_obj, method = "relative")
  head_data <- assays(fgt_rel)$counts[1:5, 1:3]
  cat("   First 5x3 of relative abundances:\n")
  print(head_data)
  
  # Filter taxa
  cat("\n   Testing filter_taxa()...\n")
  fgt_filtered <- filter_taxa(fgt_obj, min_prevalence = 0.3)
  cat("   Original taxa count:", nrow(fgt_obj), "\n")
  cat("   Filtered taxa count:", nrow(fgt_filtered), "\n")
  
  # Aggregate taxa
  cat("\n   Testing aggregate_taxa()...\n")
  fgt_agg <- aggregate_taxa(fgt_obj, rank = "Phylum")
  cat("   Aggregated taxa count:", nrow(fgt_agg), "\n")
  cat("   Aggregated taxa names (first 5):\n")
  print(head(rownames(fgt_agg), 5))
  
  # Test visualization functions
  cat("\n3. Testing visualization functions...\n")
  tryCatch({
    cat("   Testing plot_taxa_composition()...\n")
    p1 <- plot_taxa_composition(fgt_rel, rank = "Phylum", top_n = 5)
    cat("   ✓ plot_taxa_composition succeeded\n")
    
    cat("   Testing plot_alpha_diversity()...\n")
    if (requireNamespace("vegan", quietly = TRUE)) {
      p2 <- plot_alpha_diversity(fgt_obj, index = "shannon", by = "Group")
      cat("   ✓ plot_alpha_diversity succeeded\n")
    } else {
      cat("   ✕ Skipping plot_alpha_diversity (vegan package not available)\n")
    }
  }, error = function(e) {
    cat("   ✕ Error in visualization:", conditionMessage(e), "\n")
  })
  
  cat("\n4. Testing utility functions...\n")
  rank_names <- get_taxonomic_ranks(fgt_obj)
  cat("   Taxonomic ranks:", paste(rank_names, collapse = ", "), "\n")
  
  # Test conversions
  cat("\n5. Testing conversion functions...\n")
  if (requireNamespace("phyloseq", quietly = TRUE) && requireNamespace("ape", quietly = TRUE)) {
    library(phyloseq)
    
    # Create a phyloseq object
    cat("   Testing phyloseq conversion...\n")
    otu_table <- otu_table(counts, taxa_are_rows = TRUE)
    tax_table <- tax_table(taxa)
    sample_tab <- sample_data(sample_data)
    phy_tree <- phy_tree(tree)
    
    ps <- phyloseq(otu_table, tax_table, sample_tab, phy_tree)
    
    # Convert to FGTExperiment
    fgt_from_ps <- phyloseq_to_fgt(ps)
    cat("   ✓ phyloseq_to_fgt succeeded\n")
    cat("   Converted object information:\n")
    print(fgt_from_ps)
  } else {
    cat("   ✕ Skipping phyloseq conversion (phyloseq package not available)\n")
  }
  
  cat("\n\n✓ All tests completed!\n")
}, error = function(e) {
  cat("\n✕ Error in testing:", conditionMessage(e), "\n")
})