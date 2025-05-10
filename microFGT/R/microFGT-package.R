#' @title microFGT: Comprehensive Analysis of Female Genital Tract Microbiome Data
#' @description 
#' An integrated framework for analyzing microbiome data from the female genital tract.
#' This package unifies specialized tools (dada2, phyloseq, speciateIT, VALENCIA, and VIRGO)
#' within a cohesive framework that handles both amplicon and metagenomic sequencing data.
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
#' @name microFGT-package
#' @aliases microFGT
#' @import methods
#' @importFrom SummarizedExperiment assays assays<- colData colData<- rowData rowData<-
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
#' @importFrom S4Vectors SimpleList metadata metadata<-
#' @importFrom magrittr %>%
NULL