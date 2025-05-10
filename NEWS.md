# microFGT News

## microFGT 0.1.0 (2023-05-10)

### Major Changes

* Initial package release
* Complete redesign with proper R package structure
* Implemented core functionality for FGT microbiome analysis

### New Features

* Added `FGTExperiment` class extending TreeSummarizedExperiment
* Implemented basic data manipulation functions:
  * `transform_abundance()` for count transformations
  * `filter_taxa()` for feature filtering
  * `aggregate_taxa()` for taxonomic aggregation
  * `get_taxonomic_ranks()` for extracting available taxonomic levels
* Added visualization functions:
  * `plot_taxa_composition()` for taxonomic barplots
  * `plot_alpha_diversity()` for diversity metrics visualization
* Implemented data import/export:
  * `import_dada2()` for importing data from DADA2
* Added example data generation:
  * `load_example_data()` for pre-built datasets
  * `generate_fgt_example_data()` for synthetic data generation
* Included comprehensive documentation and vignettes

### Internal Changes

* Added R package infrastructure
* Implemented proper documentation using roxygen2
* Added unit tests with testthat
* Created example datasets for testing and demonstrations