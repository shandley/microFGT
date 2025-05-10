#' @importFrom Rcpp sourceCpp
#' @useDynLib microFGT, .registration = TRUE
NULL

.onLoad <- function(libname, pkgname) {
  # Package initialization code
  # Register required imports
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("."))
  }
  
  # Check for Rcpp functionality
  has_rcpp <- tryCatch({
    # Try to load Rcpp package
    if (!requireNamespace("Rcpp", quietly = TRUE)) {
      message("Rcpp package not available. Some functions may use R fallbacks.")
      return(FALSE)
    }
    
    # Check that shared libraries are loaded
    if (is.null(getLoadedDLLs()[[pkgname]])) {
      message("microFGT shared libraries not loaded. Some functions may use R fallbacks.")
      return(FALSE)
    }
    
    TRUE
  }, error = function(e) {
    message("Error checking Rcpp status: ", conditionMessage(e))
    FALSE
  })
  
  # Store in package environment
  assign("HAS_RCPP", has_rcpp, envir = topenv(parent.frame()))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("microFGT: Comprehensive Analysis of Female Genital Tract Microbiome Data")
  packageStartupMessage("Version: ", utils::packageVersion("microFGT"))
  packageStartupMessage("Status: ALPHA - early development - APIs may change")
  packageStartupMessage("For help, see ?microFGT or use browseVignettes('microFGT')")
  
  # Check for required packages
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