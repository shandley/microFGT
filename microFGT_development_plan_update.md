# microFGT Development Plan - Status Update

## Initial Phase Implementation Status

1. ✅ **Create the package skeleton with appropriate DESCRIPTION, NAMESPACE, and documentation**
   - DESCRIPTION file created with appropriate dependencies
   - Package documentation created (microFGT-package.R)
   - Basic README added
   - Directory structure set up

2. ✅ **Implement the core TreeSE-based data container**
   - Created FGTExperiment class extending TreeSummarizedExperiment
   - Added custom slots for experimentType and fgtMetadata
   - Implemented accessors and show method
   - Tests created for core functionality

3. ✅ **Develop conversion functions from common input formats (dada2, phyloseq) to TreeSE**
   - Implemented phyloseq_to_fgt() function
   - Implemented import_dada2() function
   - Added create_multiomic_fgt() for multi-omics support
   - Tests added for conversion functions

4. ✅ **Build basic data manipulation and transformation functions**
   - Implemented filter_taxa() for filtering taxa by prevalence/abundance
   - Created transform_abundance() for converting count data to different formats
   - Added aggregate_taxa() for taxonomic aggregation
   - Implemented get_taxa_abundances() and get_sample_data() for tidy data access
   - C++ implementations for performance-critical operations
   - Tests added for data manipulation functions

## Tool Integration Phase - Initial Implementation

1. ✅ **Proof-of-concept for speciateIT integration**
   - Added is_speciateit_available() to check for tool installation
   - Implemented default_speciateit_db() for database path resolution
   - Created run_speciateit() wrapper for command-line interface
   - Implemented classify_with_speciateit() for integrating with FGTExperiment
   - Added helper functions for installation and database setup

## Additional Features Implemented

1. ✅ **Basic visualization functions**
   - Implemented plot_taxa_composition() for taxonomic barplots
   - Added plot_alpha_diversity() for diversity visualization

2. ✅ **Utility functions**
   - Added package requirement checking utilities
   - Path normalization for cross-platform compatibility
   - Tool installation and database setup placeholders

3. ✅ **Documentation**
   - Created introductory vignette
   - Added comprehensive function documentation
   - Included examples for all exported functions

## Next Steps

### Tool Integration Phase (Remaining)

1. ⬜ **Create R implementation or reticulate wrapper for VALENCIA (community state typing)**
   - Implement R-based VALENCIA functions or reticulate wrapper
   - Create assign_cst() function for community state typing
   - Add visualization functions for CST results

2. ⬜ **Develop integration with VIRGO (vaginal gene catalog and analysis pipeline)**
   - Create run_virgo_mapping() wrapper
   - Implement import_virgo() function
   - Add assign_mgcst() for metagenomic CST assignment

3. ⬜ **Create parsers to convert all tool outputs to TreeSE-compatible format**
   - Standardize output formats across tools
   - Implement parsers for specific file formats

### Analysis Module Phase

1. ⬜ **Implement taxonomic analysis functions (filtering, aggregation, visualization)**
   - Add advanced taxonomic analysis functions
   - Implement statistical tests for differential abundance

2. ⬜ **Build community typing module with VALENCIA integration**
   - Add functions for CST transition analysis
   - Implement visualization functions for CST distributions

3. ⬜ **Develop metagenomic analysis module with VIRGO integration**
   - Add functions for gene catalog analysis
   - Implement pathway enrichment analysis

4. ⬜ **Create multi-omics integration module for correlating amplicon and metagenomic data**
   - Add correlation analysis functions
   - Implement visualization functions for multi-omics data

## Technical Achievements

1. ✅ **Efficient memory usage**
   - TreeSummarizedExperiment as base class provides efficient storage
   - C++ implementations for performance-critical operations

2. ✅ **Tidyverse compatibility**
   - All functions accept objects as first argument for pipe compatibility
   - Tidy data extraction functions return tibbles

3. ✅ **Bioconductor compatibility**
   - Based on Bioconductor data structures
   - Following Bioconductor coding standards
   - Compatible with MultiAssayExperiment for multi-omics integration