#' @keywords internal
"_PACKAGE"

#' @import methods
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom SummarizedExperiment SummarizedExperiment assays assayNames colData rowData
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment rowTree
#' @importFrom Biostrings DNAStringSet
#' @importFrom Rcpp sourceCpp
#' @importFrom stats rnorm runif
#' @importFrom utils packageVersion
#' @importFrom dplyr filter group_by mutate select summarize
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal theme element_text
#' @importFrom magrittr %>%
#' @useDynLib microFGT, .registration = TRUE
NULL