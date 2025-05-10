#' Plot taxonomic composition
#'
#' Creates a stacked bar plot showing the taxonomic composition of samples.
#'
#' @param fgt_exp An FGTExperiment object
#' @param rank Taxonomic rank to plot (e.g., "Genus", "Family")
#' @param top_n Number of top taxa to show individually (remaining are grouped as "Other")
#' @param assay_name Name of the assay to use (preferably a normalized one like "relative")
#' @param group_var Optional grouping variable from colData
#' @param sort_by How to sort samples: "none", "abundance" (by dominant taxa), or "group" (by group_var)
#' @param colors Vector of colors for taxa, or a color palette name
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot genus-level composition
#' plot_taxa_composition(fgt_exp, rank = "Genus", top_n = 10,
#'                       assay_name = "relative", group_var = "group")
#' }
plot_taxa_composition <- function(fgt_exp, rank, top_n = 10, assay_name = "relative", 
                                 group_var = NULL, sort_by = "abundance",
                                 colors = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for this function")
  }
  
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required for this function")
  }
  
  # Input validation
  if (!methods::is(fgt_exp, "FGTExperiment")) {
    stop("Input must be an FGTExperiment object")
  }
  
  if (!assay_name %in% names(SummarizedExperiment::assays(fgt_exp))) {
    # If relative abundance not available, create it
    if (assay_name == "relative" && "counts" %in% names(SummarizedExperiment::assays(fgt_exp))) {
      message("Creating relative abundance assay from counts...")
      fgt_exp <- transform_abundance(fgt_exp, type = "relative", assay_name = "counts")
    } else {
      stop(paste0("Assay '", assay_name, "' not found in the FGTExperiment object"))
    }
  }
  
  # Check if we need to aggregate data at the specified rank
  rowdata <- SummarizedExperiment::rowData(fgt_exp)
  if (!rank %in% colnames(rowdata)) {
    stop(paste0("Taxonomic rank '", rank, "' not found in rowData"))
  }
  
  # Prepare data for plotting
  abundances <- get_taxa_abundances(fgt_exp, assay_name = assay_name, include_metadata = TRUE)
  
  # Convert to long format
  abundance_long <- tidyr::pivot_longer(
    abundances, 
    cols = dplyr::starts_with("Sample"), 
    names_to = "sample_id", 
    values_to = "abundance"
  )
  
  # Group by rank and sample, sum abundances
  taxa_by_sample <- abundance_long %>%
    dplyr::group_by(sample_id, !!as.symbol(rank)) %>%
    dplyr::summarize(abundance = sum(abundance), .groups = "drop") %>%
    dplyr::ungroup()
  
  # Identify top taxa
  top_taxa <- taxa_by_sample %>%
    dplyr::group_by(!!as.symbol(rank)) %>%
    dplyr::summarize(total_abundance = sum(abundance), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total_abundance)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(!!as.symbol(rank))
  
  # Replace non-top taxa with "Other"
  taxa_by_sample <- taxa_by_sample %>%
    dplyr::mutate(
      displayed_taxon = ifelse(!!as.symbol(rank) %in% top_taxa, 
                               !!as.symbol(rank), 
                               "Other")
    )
  
  # Re-calculate abundance for grouped "Other" taxa
  taxa_by_sample <- taxa_by_sample %>%
    dplyr::group_by(sample_id, displayed_taxon) %>%
    dplyr::summarize(abundance = sum(abundance), .groups = "drop") %>%
    dplyr::ungroup()
  
  # Add grouping information if provided
  if (!is.null(group_var)) {
    if (!group_var %in% colnames(SummarizedExperiment::colData(fgt_exp))) {
      stop(paste0("Group variable '", group_var, "' not found in colData"))
    }
    
    # Extract grouping information
    sample_data <- get_sample_data(fgt_exp)
    
    # Join with abundance data
    taxa_by_sample <- dplyr::left_join(
      taxa_by_sample,
      sample_data %>% dplyr::select(sample_id, !!as.symbol(group_var)),
      by = "sample_id"
    )
  }
  
  # Sort samples if requested
  if (sort_by == "abundance") {
    # Get dominant taxon for each sample
    dominant_taxa <- taxa_by_sample %>%
      dplyr::group_by(sample_id) %>%
      dplyr::arrange(dplyr::desc(abundance), .by_group = TRUE) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
    
    # Determine sample order based on dominant taxa and their abundance
    sample_order <- dominant_taxa %>%
      dplyr::arrange(displayed_taxon, dplyr::desc(abundance)) %>%
      dplyr::pull(sample_id)
    
    # Convert sample_id to factor with ordered levels
    taxa_by_sample$sample_id <- factor(taxa_by_sample$sample_id, levels = sample_order)
  } else if (sort_by == "group" && !is.null(group_var)) {
    # Determine sample order based on group
    sample_order <- sample_data %>%
      dplyr::arrange(!!as.symbol(group_var), sample_id) %>%
      dplyr::pull(sample_id)
    
    # Convert sample_id to factor with ordered levels
    taxa_by_sample$sample_id <- factor(taxa_by_sample$sample_id, levels = sample_order)
  }
  
  # Generate color palette if not provided
  if (is.null(colors)) {
    # Use default ggplot2 colors
    num_colors_needed <- length(unique(taxa_by_sample$displayed_taxon))
    colors <- scales::hue_pal()(num_colors_needed)
  }
  
  # Create the plot
  plot <- ggplot2::ggplot(taxa_by_sample, ggplot2::aes(x = sample_id, y = abundance, 
                                               fill = displayed_taxon)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_fill_manual(values = colors, name = rank) +
    ggplot2::labs(
      x = "Sample",
      y = "Abundance",
      title = paste("Taxonomic composition at", rank, "level")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
  
  # Add faceting by group if requested
  if (!is.null(group_var)) {
    plot <- plot + ggplot2::facet_grid(. ~ !!as.symbol(group_var), scales = "free_x", space = "free")
  }
  
  return(plot)
}

#' Plot alpha diversity
#'
#' Creates box plots of alpha diversity metrics across groups.
#'
#' @param fgt_exp An FGTExperiment object
#' @param metrics Vector of diversity metrics to calculate: "observed" (richness), 
#'                "shannon", "simpson", "invsimpson" (inverse Simpson)
#' @param group_var Grouping variable from colData (or NULL for all samples)
#' @param assay_name Name of the count assay to use
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot alpha diversity by group
#' plot_alpha_diversity(fgt_exp, metrics = c("observed", "shannon"), 
#'                      group_var = "group")
#' }
plot_alpha_diversity <- function(fgt_exp, metrics = c("observed", "shannon"), 
                                group_var = NULL, assay_name = "counts") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for this function")
  }
  
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required for this function")
  }
  
  # Input validation
  if (!methods::is(fgt_exp, "FGTExperiment")) {
    stop("Input must be an FGTExperiment object")
  }
  
  if (!assay_name %in% names(SummarizedExperiment::assays(fgt_exp))) {
    stop(paste0("Assay '", assay_name, "' not found in the FGTExperiment object"))
  }
  
  if (!all(metrics %in% c("observed", "shannon", "simpson", "invsimpson"))) {
    stop("Metrics must be one or more of: 'observed', 'shannon', 'simpson', 'invsimpson'")
  }
  
  # Extract count matrix
  counts <- SummarizedExperiment::assays(fgt_exp)[[assay_name]]
  
  # Calculate diversity metrics
  diversity_values <- data.frame(sample_id = colnames(counts))
  
  if ("observed" %in% metrics) {
    # Observed richness (number of non-zero features)
    diversity_values$observed <- colSums(counts > 0)
  }
  
  if (any(c("shannon", "simpson", "invsimpson") %in% metrics)) {
    # Calculate relative abundances for Shannon and Simpson indices
    rel_abund <- counts / rep(colSums(counts), each = nrow(counts))
    rel_abund[is.na(rel_abund)] <- 0
    
    if ("shannon" %in% metrics) {
      # Shannon diversity: -sum(p_i * log(p_i))
      # Replace 0 with tiny value to avoid log(0)
      log_rel <- log(rel_abund + 1e-10)
      log_rel[rel_abund == 0] <- 0  # Set log of 0 to 0
      diversity_values$shannon <- -colSums(rel_abund * log_rel)
    }
    
    if ("simpson" %in% metrics || "invsimpson" %in% metrics) {
      # Simpson diversity: 1 - sum(p_i^2)
      simpson <- 1 - colSums(rel_abund^2)
      
      if ("simpson" %in% metrics) {
        diversity_values$simpson <- simpson
      }
      
      if ("invsimpson" %in% metrics) {
        # Inverse Simpson: 1 / (1 - sum(p_i^2))
        diversity_values$invsimpson <- 1 / (1 - simpson)
      }
    }
  }
  
  # Add grouping information if provided
  if (!is.null(group_var)) {
    if (!group_var %in% colnames(SummarizedExperiment::colData(fgt_exp))) {
      stop(paste0("Group variable '", group_var, "' not found in colData"))
    }
    
    # Extract grouping information
    sample_data <- get_sample_data(fgt_exp)
    
    # Join with diversity data
    diversity_values <- dplyr::left_join(
      diversity_values,
      sample_data %>% dplyr::select(sample_id, !!as.symbol(group_var)),
      by = "sample_id"
    )
  }
  
  # Convert to long format for plotting
  diversity_long <- tidyr::pivot_longer(
    diversity_values,
    cols = metrics,
    names_to = "metric",
    values_to = "value"
  )
  
  # Create factor with nice labels for metrics
  metric_labels <- c(
    "observed" = "Observed Richness",
    "shannon" = "Shannon Diversity",
    "simpson" = "Simpson Diversity",
    "invsimpson" = "Inverse Simpson Diversity"
  )
  
  diversity_long$metric <- factor(
    diversity_long$metric,
    levels = metrics,
    labels = metric_labels[metrics]
  )
  
  # Create the plot
  if (is.null(group_var)) {
    # Simple boxplot of diversity values
    plot <- ggplot2::ggplot(diversity_long, ggplot2::aes(x = metric, y = value)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_jitter(width = 0.2, alpha = 0.6) +
      ggplot2::labs(
        x = NULL,
        y = "Diversity Value",
        title = "Alpha Diversity Metrics"
      ) +
      ggplot2::theme_minimal()
  } else {
    # Boxplot grouped by group_var
    plot <- ggplot2::ggplot(diversity_long, 
                           ggplot2::aes(x = !!as.symbol(group_var), y = value, fill = !!as.symbol(group_var))) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.2, alpha = 0.6) +
      ggplot2::facet_wrap(~ metric, scales = "free_y") +
      ggplot2::labs(
        x = NULL,
        y = "Diversity Value",
        title = "Alpha Diversity Metrics by Group"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom"
      )
  }
  
  return(plot)
}