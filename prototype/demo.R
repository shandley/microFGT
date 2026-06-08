# prototype/demo.R
# Assemble three tool outputs into one MultiAssayExperiment, then show the payoff.
suppressPackageStartupMessages(library(MultiAssayExperiment))
source("prototype/importers.R")

mock <- "prototype/mock"

# --- step 3: import each tool's output to a standard object ------------------
taxa <- import_speciateit(file.path(mock, "speciateit", "MC_order7_results.txt"))
genes <- import_virgo(
  file.path(mock, "virgo", "mock_virgo_test.out"),
  file.path(mock, "virgo", "mock_virgo_genes.txt")
)
cst  <- import_valencia(file.path(mock, "valencia", "mock_valencia_cst.csv"))

cat("imported:\n")
cat(sprintf("  taxa (speciateIT): %d taxa x %d samples\n", nrow(taxa), ncol(taxa)))
cat(sprintf("  genes (VIRGO)    : %d genes x %d samples\n", nrow(genes), ncol(genes)))
cat(sprintf("  CST (VALENCIA)   : %d samples\n\n", nrow(cst)))

# --- step 4: assemble into a MultiAssayExperiment ----------------------------
# colData = sample-level metadata (CST) keyed by sample. MAE builds the
# sampleMap from matching colnames and reconciles the two sample sets itself.
mae <- MultiAssayExperiment(
  experiments = ExperimentList(taxa = taxa, `function` = genes),
  colData = cst
)
cat("=== MultiAssayExperiment ===\n"); print(mae)

# --- step 5: the payoff ------------------------------------------------------
# 5a. samples assayed by BOTH modalities (amplicon broad, metagenomics subset)
both <- intersectColumns(mae)
cat(sprintf("\nintersectColumns(): %d samples have BOTH taxa and genes:\n  %s\n",
            ncol(both[[1]]), paste(colnames(both)[[1]], collapse = ", ")))

# 5b. subset to compare CST-IV* vs CST-I, on the both-modality samples
cst_both <- colData(both)$CST
cat("\nCST of dual-modality samples:\n"); print(table(cst_both))

is_iv <- grepl("^IV", cst_both); is_i <- cst_both == "I"
ivc <- colnames(both)[[2]][is_iv]   # column 2 = the 'function' (genes) experiment
ic  <- colnames(both)[[2]][is_i]

genes_both <- assay(both[["function"]])
mean_genes_detected <- function(cols) if (length(cols)) mean(colSums(genes_both[, cols, drop = FALSE] > 0)) else NA
cat(sprintf(
  "\nCST-IV* (n=%d) vs CST-I (n=%d): mean VIRGO genes detected/sample = %.0f vs %.0f\n",
  length(ivc), length(ic), mean_genes_detected(ivc), mean_genes_detected(ic)
))
cat("(CST-IV = dysbiotic/diverse -> expect more functional genes than L. crispatus-dominated CST-I)\n")
