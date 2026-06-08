# prototype/test_importers.R
# Contract check: each importer must parse BOTH the real-shaped fixture AND the
# fixed mock output into a valid, same-shaped object. This is the forcing
# function the mock never had: the SAME importer reads real and mock, or fails.
suppressPackageStartupMessages(library(MultiAssayExperiment))
source("prototype/importers.R")

FX <- "prototype/real_fixtures"
ok <- TRUE
check <- function(label, cond) {
  cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
  if (!isTRUE(cond)) ok <<- FALSE
}

cat("== REAL fixtures ==\n")

# VIRGO: genuine per-sample outputs (ravel-lab/VIRGO _test_run).
vdir <- tempfile("virgo_real_"); dir.create(vdir)
invisible(file.copy(file.path(FX, "virgo_sub1.out"), file.path(vdir, "sub1.out")))
invisible(file.copy(file.path(FX, "virgo_sub2.out"), file.path(vdir, "sub2.out")))
v_real <- import_virgo(vdir)
check("VIRGO real: genes x 2 samples", ncol(v_real) == 2 && nrow(v_real) > 1000)
check("VIRGO real: integer counts, zero-filled (no NA)", !anyNA(assay(v_real)))

# speciateIT: genuine ASV count table (169 x 1514) + real-SHAPED results over its ASVs.
ct_path <- file.path(FX, "speciateit_test_count_table.csv")
asvs <- colnames(read.csv(ct_path, row.names = 1, check.names = FALSE, nrows = 1))
set.seed(1)
pool <- c("Lactobacillus crispatus", "Lactobacillus iners", "Gardnerella vaginalis",
          "Atopobium vaginae", "Prevotella bivia")
res_path <- tempfile("MC_", fileext = ".txt")
write.table(data.frame(
  "Sequence ID"           = asvs,
  "Classification"        = sample(pool, length(asvs), replace = TRUE),
  "posterior probability" = runif(length(asvs), 0.7, 1.0),
  "number of Decisions"   = sample(5:20, length(asvs), replace = TRUE),
  check.names = FALSE), res_path, sep = "\t", quote = FALSE, row.names = FALSE)
t_real <- import_speciateit(res_path, ct_path)
check("speciateIT real: taxa x 169 samples (ASV->taxon join)",
      ncol(t_real) == 169 && nrow(t_real) <= length(pool) + 1)

# VALENCIA: real-SHAPED output fixture (no genuine VALENCIA run is committed).
subs <- c("I-A", "III-A", "IV-A")
sim <- matrix(0.0125, 3, 13, dimnames = list(NULL,
        paste0(c("I-A","I-B","II","III-A","III-B","IV-A","IV-B",
                 "IV-C0","IV-C1","IV-C2","IV-C3","IV-C4","V"), "_sim")))
for (i in 1:3) sim[i, paste0(subs[i], "_sim")] <- 0.85
val_real <- tempfile("valencia_", fileext = ".csv")
write.csv(data.frame(
  sampleID = c("s1","s2","s3"), read_count = c(10000,10000,10000),
  Lactobacillus_crispatus = c(0.9,0.1,0.0), Gardnerella_vaginalis = c(0.0,0.1,0.6),
  as.data.frame(sim, check.names = FALSE),
  subCST = subs, score = apply(sim,1,max), CST = c("I","III","IV-A"),
  check.names = FALSE, row.names = NULL), val_real, quote = FALSE, row.names = FALSE)
c_real <- import_valencia(val_real)
check("VALENCIA real: 3 samples, CST/subCST/score pulled from trailing cols",
      nrow(c_real) == 3 && all(c("CST","subCST","score") %in% names(c_real)) &&
      identical(as.character(c_real$CST), c("I","III","IV-A")))

cat("== MOCK via fixed writers ==\n")
source("R/mock_data.R")
md <- tempfile("mock_")
invisible(capture.output(suppressMessages(
  generate_mock_fgt_dataset(n_samples = 10, n_sequences = 600, n_genes = 300,
                            seed = 42, create_files = TRUE, output_dir = md))))
t_mock <- import_speciateit(file.path(md, "speciateit", "MC_order7_results.txt"),
                            file.path(md, "speciateit", "ASV_count_table.csv"))
v_mock <- import_virgo(file.path(md, "virgo"))
c_mock <- import_valencia(file.path(md, "valencia", "mock_valencia_output.csv"))
check("speciateIT mock: same importer, taxa x 10", ncol(t_mock) == 10)
check("VIRGO mock: same importer reads per-sample .out, genes x 10", ncol(v_mock) == 10)
check("VALENCIA mock: same importer, 10 samples w/ CST",
      nrow(c_mock) == 10 && !anyNA(c_mock$CST))

cat("== Assemble + payoff (mock) ==\n")
mae <- MultiAssayExperiment(ExperimentList(taxa = t_mock, `function` = v_mock),
                            colData = c_mock)
check("MAE assembles (2 experiments)", length(experiments(mae)) == 2)
both <- intersectColumns(mae)
check("intersectColumns() yields shared samples", ncol(both[[1]]) >= 1)

cat(if (ok) "\nALL CHECKS PASSED\n" else "\nSOME CHECKS FAILED\n")
quit(status = if (ok) 0 else 1)
