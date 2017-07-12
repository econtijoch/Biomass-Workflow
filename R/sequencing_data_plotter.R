#' Make abundance bar plots of sequencing data
#'
#' @param sequencing_object sequencing object (e.g. result of individual_scale)
#' @param depth taxonomic depth to plot
#' @param x.var variable for x axis
#' @param abundance relative or absolute
#' @param x.groups grouping variable for faceting along x axis
#' @param y.groups grouping variable for faceting along y axis
#' @param facet.scales scales for facets (e.g. free)
#' @param facet.space space for facets (e.g. free)
#' @param colors colors to use for bars
#' @param tilt.axis tilt x axis?
#' @param x.axis.factor Make x axis variable factor?
#' @param wrap.groups grouping variable to facet_wrap
#'
#' @return ggplot object
#' @export

sequencing_data_plotter <- function(sequencing_object, depth = 'Phylum', x.var = 'X.SampleID', abundance = 'relative', wrap.groups = NULL, x.groups = NULL, y.groups = NULL, facet.scales = 'free', facet.space = 'free', colors = NULL, tilt.axis = T, x.axis.factor = F) {
  
  if (abundance == 'relative') {
    data <- dplyr::left_join(sequencing_object$melted_relative_by_taxonomy[[depth]], sequencing_object$sample_metadata)
    ylabel <- 'Relative Abundance\n(percentage of mapped 16S reads)'
  } else if (abundance == 'absolute') {
    data <- dplyr::left_join(sequencing_object$melted_scaled_by_taxonomy[[depth]], sequencing_object$sample_metadata)
    ylabel <- paste0('Absolute Abundance \n(\u03bcg DNA per ', depth, ' per mg sample)')
  }
  
  
  if (is.null(y.groups)) {
    facet_row <- "."
  } else {
    facet_row <- y.groups
  }
  if (is.null(x.groups)) {
    facet_col <- "."
  } else {
    facet_col <- x.groups
  }
  if (facet_row != "." & facet_col != ".") {
    data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", depth, x.var, x.groups, y.groups)
  } else if (facet_col != ".") {
    data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", depth, x.var, x.groups)
  } else if (facet_row != ".") {
    data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", depth, x.var, y.groups)
  } else {
    data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", depth, x.var)
  }
  
  if (!is.null(wrap.groups)) {
	  data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", depth, x.var, wrap.groups)
  }
  
  plot_data <- data %>% dplyr::summarize(mean_abundance = mean(abundance)/n()) %>% dplyr::left_join(., sequencing_object$sample_metadata)
  
  
  if (x.axis.factor) {
	  plot_data[[x.var]] <- as.factor(plot_data[[x.var]])
  } 
  xval <- x.var
  
  yval <- 'mean_abundance'
  filling <- 'long_label'
  taxa_label_pre <- stringr::str_split(plot_data$long_label, pattern = '__', simplify = T)
  taxa_label <- as.character(taxa_label_pre[,ncol(taxa_label_pre)])
  
  
  
  plot <- ggplot2::ggplot(data = plot_data, ggplot2::aes_string(x = xval, y = yval, fill = filling)) + ggplot2::geom_bar(stat = 'identity') + ggplot2::coord_cartesian(expand = FALSE) + ggplot2::labs(y = ylabel, x = NULL)
  
  if (is.null(colors)) {
    plot <- plot + ggplot2::scale_fill_manual(name = depth, breaks = plot_data$long_label,  labels = taxa_label, values = BiomassWorkflow::EJC_colors)
  } else {
    plot <- plot + ggplot2::scale_fill_manual(name = depth, breaks = plot_data$long_label,  labels = taxa_label, values = colors)
  }
  
  
  
  if (!is.null(facet_row) & !is.null(facet_col)) {
    facets <- paste(facet_row, '~', facet_col, sep = " ")
    if (facets != '. ~ .') {
      plot <- plot + ggplot2::facet_grid(facets, scales = facet.scales, space = facet.space)
    }
  }
  if (!is.null(wrap.groups)) {
	  plot <- plot + ggplot2::facet_wrap(paste0('~ ', wrap.groups), scales = facet.scales)
  }
  
  if (tilt.axis) {
    output <- plot + paper_theme_tilted()
  } else {
    output <- plot + paper_theme()
  }
  return(output)
  
}


