# microFGT Example Data Generation: Implementation Guide

## Overview

This document outlines the implementation plan for creating realistic example data generation functions for the microFGT package. The goal is to develop flexible utilities that create synthetic datasets mimicking real female genital tract (FGT) microbiome characteristics for testing, documentation, and educational purposes.

## Key Features

The example data generation should:

1. Produce realistic FGT microbiome profiles with appropriate taxonomic structures
2. Generate consistent pairs of sequence tables, taxonomic assignments, and metadata
3. Allow parameterization of key properties (sample size, diversity, community types)
4. Include relevant clinical metadata fields common in FGT research
5. Support both amplicon and metagenomics data structures
6. Provide options for various levels of data complexity

## Implementation Components

### 1. Core Data Generation Functions

#### `generate_fgt_example_data()`
Main function to generate a complete set of example data with configurable parameters:

```r
generate_fgt_example_data(
  n_samples = 20,
  n_features = 100,
  sample_groups = c("Healthy", "BV"),
  group_proportions = c(0.7, 0.3),
  community_types = c("CST-I", "CST-III", "CST-IV"),
  sequencing_depth = c(5000, 20000),
  include_tree = TRUE,
  random_seed = NULL,
  output_dir = NULL,
  format = c("FGTExperiment", "rds", "csv")
)
```

#### `create_fgt_count_matrix()`
Generate a count matrix with realistic FGT taxa distributions:

```r
create_fgt_count_matrix(
  n_samples = 20,
  n_features = 100,
  community_types = c("CST-I", "CST-III", "CST-IV"),
  community_proportions = NULL,
  sequencing_depth = c(5000, 20000),
  random_seed = NULL
)
```

#### `create_fgt_taxonomy()`
Generate taxonomic classifications typical of FGT microbiome:

```r
create_fgt_taxonomy(
  feature_ids,
  community_types = c("CST-I", "CST-III", "CST-IV"),
  include_rare_taxa = TRUE,
  taxonomic_levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
)
```

#### `create_fgt_sample_metadata()`
Generate clinical and technical metadata for FGT samples:

```r
create_fgt_sample_metadata(
  sample_ids,
  group_variable = "condition",
  groups = c("Healthy", "BV"),
  group_proportions = c(0.7, 0.3),
  include_clinical = TRUE,
  include_technical = TRUE,
  community_types = NULL
)
```

### 2. Helper Functions and Constants

#### `create_fgt_phylogenetic_tree()`
Generate a phylogenetic tree matching the taxonomy:

```r
create_fgt_phylogenetic_tree(
  feature_ids,
  taxonomy_table,
  method = c("random", "taxonomy-based")
)
```

#### Constants for FGT Taxa Composition

Define realistic taxa compositions for different community state types (CSTs):

```r
# CST-I: Dominated by L. crispatus
CST_I_TAXA <- list(
  dominant = list(
    c("Bacteria", "Firmicutes", "Bacilli", "Lactobacillales", "Lactobacillaceae", "Lactobacillus", "Lactobacillus crispatus"),
    proportion = 0.70, 0.95)  # 70-95% of reads
  ),
  subdominant = list(...),
  rare = list(...)
)

# CST-III: Dominated by L. iners
CST_III_TAXA <- ...

# CST-IV: Diverse anaerobes (BV-associated)
CST_IV_TAXA <- ...

# CST-V: Dominated by L. jensenii
CST_V_TAXA <- ...
```

#### Constants for Clinical Metadata

Define realistic clinical parameter distributions:

```r
CLINICAL_PARAMS <- list(
  "pH" = list(
    "Healthy" = list(mean = 4.2, sd = 0.4, min = 3.8, max = 4.5),
    "BV" = list(mean = 5.2, sd = 0.5, min = 4.5, max = 6.0)
  ),
  "Nugent_Score" = list(
    "Healthy" = list(mean = 1.5, sd = 1.0, min = 0, max = 3),
    "BV" = list(mean = 7.5, sd = 1.5, min = 4, max = 10)
  ),
  # Additional relevant clinical parameters
)
```

### 3. Utility Functions

#### `save_example_data()`
Export generated data in various formats:

```r
save_example_data(
  seq_tab, 
  taxonomy, 
  metadata, 
  tree = NULL,
  output_dir = ".",
  base_name = "microFGT_example",
  formats = c("rds", "csv", "newick")
)
```

#### `load_example_data()`
Simplified function to load the built-in example datasets:

```r
load_example_data(
  size = c("small", "medium", "large"),
  type = c("amplicon", "metagenomics"),
  as_fgt_experiment = TRUE
)
```

## Development Steps

1. **Phase 1: Basic Infrastructure**
   - Implement core data structure generation
   - Create realistic taxonomic templates for different CSTs
   - Develop basic sample metadata generation
   - Create simple validation functions

2. **Phase 2: Biological Realism**
   - Refine taxa distributions based on published literature
   - Implement correlation structures between clinical parameters
   - Add realistic noise and variability
   - Create phylogenetic tree generation

3. **Phase 3: Integration and Packaging**
   - Pre-generate standard example datasets
   - Package datasets within inst/extdata/
   - Create comprehensive documentation
   - Develop tutorial vignette showing use cases

4. **Phase 4: Advanced Features**
   - Add longitudinal data simulation
   - Implement batch effects
   - Create multi-omics example data
   - Add specialized FGT tool outputs (speciateIT, VALENCIA)

## Testing Plan

### Unit Tests

1. **Function-level Tests**
   - Test each generator function independently
   - Verify parameter validation
   - Test edge cases (empty datasets, single samples, etc.)

2. **Output Validation Tests**
   - Verify taxonomic consistency
   - Check clinical parameter distributions
   - Validate correlation structures
   - Confirm community type characteristics

3. **Integration Tests**
   - Test compatibility with core microFGT functions
   - Verify data import/export functions
   - Test visualization capabilities with example data

### Example Scripts

Create example scripts in `inst/examples/` demonstrating:
1. Creating and manipulating example data
2. Customizing example data parameters
3. Using example data with core analysis functions
4. Comparing different community types

## Documentation 

### Help Files
- Create detailed R documentation for all public functions
- Include examples showing common use cases
- Document parameters and return values thoroughly

### Vignette
Create a dedicated vignette: "Generating and Using Example Data":
- Basic usage examples
- Parameter customization
- Comparison with real-world data
- Educational examples of FGT microbiome characteristics

## Implementation Guidelines

1. **Naming Conventions**
   - Functions: `generate_fgt_*`, `create_fgt_*`
   - Datasets: `example_fgt_*`
   - Constants: `UPPERCASE_WITH_UNDERSCORES`

2. **Code Organization**
   - Place all generator functions in `R/example_data.R`
   - Place constant definitions in `R/example_data_constants.R`
   - Package built-in data in `data/` with appropriate documentation

3. **Dependencies**
   - Minimize external dependencies
   - Use base R where possible for data generation
   - Reuse existing microFGT functions where appropriate

4. **Performance Considerations**
   - Optimize for medium-sized datasets (up to ~1000 samples)
   - Cache intermediate results for large datasets
   - Provide progress feedback for time-consuming operations

## Initial Implementation: Priority Functions

For the first implementation phase, focus on:

1. `generate_fgt_example_data()` - Main wrapper function
2. `create_fgt_count_matrix()` - Core count data generator
3. `create_fgt_taxonomy()` - Basic taxonomic structure
4. `create_fgt_sample_metadata()` - Simple metadata generation
5. Constants for CST-I, CST-III, and CST-IV taxa compositions
6. Basic testing functions to validate output structure

## References

Include key references for FGT microbiome compositions and community state types:

1. Ravel, J., et al. (2011). Vaginal microbiome of reproductive-age women. PNAS, 108(Suppl 1), 4680-4687.
2. France, M. T., et al. (2020). VALENCIA: a nearest centroid classification method for vaginal microbial communities based on composition. Microbiome, 8(1), 166.
3. Serrano, M. G., et al. (2019). Racioethnic diversity in the dynamics of the vaginal microbiome during pregnancy. Nature Medicine, 25(6), 1001-1011.
4. Brooks, J. P., et al. (2017). Effects of combined oral contraceptives, depot medroxyprogesterone acetate and the levonorgestrel-releasing intrauterine system on the vaginal microbiome. Contraception, 95(4), 405-413.