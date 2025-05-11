.onLoad <- function(libname, pkgname) {
  # Register required imports
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("."))
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("microFGT: Comprehensive Analysis of Female Genital Tract Microbiome Data")
  packageStartupMessage("Version: ", utils::packageVersion("microFGT"))
  packageStartupMessage("Status: ALPHA - early development - APIs may change")
  
  # Check for required Bioconductor packages
  bioc_deps <- c("SummarizedExperiment", "TreeSummarizedExperiment", "S4Vectors")
  missing_deps <- bioc_deps[!sapply(bioc_deps, requireNamespace, quietly = TRUE)]
  
  if (length(missing_deps) > 0) {
    packageStartupMessage(
      "Some required Bioconductor packages not found: ", 
      paste(missing_deps, collapse = ", "), 
      "\nInstall with: BiocManager::install(c('", 
      paste(missing_deps, collapse = "', '"), "'))"
    )
  }
}