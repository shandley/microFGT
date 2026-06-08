# prototype/importers.R
# Thin importers: one tool output file -> one standard Bioconductor object.
# Each importer's ONLY job is the FGT-specific reshape; the container is generic.

suppressPackageStartupMessages({
  library(SummarizedExperiment)
  library(TreeSummarizedExperiment)
})

# --- speciateIT --------------------------------------------------------------
# Input : MC_order7_results.txt, ONE ROW PER SEQUENCE. The sample is not a
#         column; it is encoded in the "Sequence ID" prefix (Sample_001_Seq_...).
# Output: a TreeSummarizedExperiment, taxa (rows) x samples (cols), assay =
#         per-sample read counts per taxon. rowData carries the taxonomy.
# GLUE  : (a) recover sample from the sequence ID, (b) aggregate seqs -> counts.
import_speciateit <- function(path, sample_from_id = function(id) sub("_Seq_.*$", "", id)) {
  df <- read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  sample <- sample_from_id(df[["Sequence ID"]])
  taxon  <- df[["Classification"]]
  counts <- as.matrix(table(taxon, sample))                 # taxa x samples
  storage.mode(counts) <- "integer"

  taxonomy <- DataFrame(
    Classification = rownames(counts),
    Genus = sub(" .*$", "", rownames(counts)),
    row.names = rownames(counts)
  )
  TreeSummarizedExperiment(
    assays  = list(counts = counts),
    rowData = taxonomy
  )
}

# --- VIRGO -------------------------------------------------------------------
# Input : <base>_test.out, LONG format (gene_id, read_count, gene_length, Sample),
#         only nonzero entries present; plus <base>_genes.txt annotation.
# Output: a SummarizedExperiment, genes (rows) x samples (cols), assay = counts
#         (absent gene/sample pairs filled with 0). rowData carries gene length +
#         function group.
# GLUE  : pivot long -> wide and zero-fill.
import_virgo <- function(test_out, genes_txt = NULL) {
  long <- read.delim(test_out, check.names = FALSE, stringsAsFactors = FALSE)
  m <- xtabs(read_count ~ gene_id + Sample, data = long)    # genes x samples
  counts <- as.matrix(unclass(m))
  storage.mode(counts) <- "integer"

  rd <- DataFrame(gene_id = rownames(counts), row.names = rownames(counts))
  if (!is.null(genes_txt) && file.exists(genes_txt)) {
    ann <- read.delim(genes_txt, check.names = FALSE, stringsAsFactors = FALSE)
    i <- match(rownames(counts), ann$gene_id)
    rd$length         <- ann$length[i]
    rd$function_group <- ann$function_group[i]
  }
  SummarizedExperiment(assays = list(counts = counts), rowData = rd)
}

# --- VALENCIA ----------------------------------------------------------------
# Input : <base>_cst.csv (Sample, CST) -- already one row per sample.
# Output: a DataFrame of per-sample labels, keyed by sample, for MAE colData.
# GLUE  : essentially none -- VALENCIA already speaks "per sample".
import_valencia <- function(cst_csv) {
  d <- read.csv(cst_csv, stringsAsFactors = FALSE)
  DataFrame(CST = d$CST, row.names = d$Sample)
}
