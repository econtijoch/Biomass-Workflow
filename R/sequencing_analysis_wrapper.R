#' Sequencing Analysis Wrapper
#'
#' @param biom_file otu table in biom format
#' @param metadata_file mapping file (QIIME-ready)
#' @param taxonomy_file file that contains taxonomy (e.g. greengenes)
#' @param tree_file taxonomic tree information (e.g. greengenes)
#' @param method dimensionality reduction method (e.g. PCoA, NMDS, DCA)
#' @param distance distance metric to use (e.g. unifrac, bray, jaccard)
#' @param n_axes number of axes to get from PCoA
#'
#' @return list containing individual components of analyzed data ready for plotting, etc.
#' @export


sequencing_analysis_wrapper <- function(biom_file, metadata_file, taxonomy_file, tree_file,  method = 'PCoA', distance = 'unifrac', n_axes = 10) {
  
  sequencing_object <- individual_scale(biom_file = biom_file, metadata_file = metadata_file, taxonomy_file = taxonomy_file)
  message('Completed Generating BiomassWorkflow sequencing object...')
  
  phyloseq_object <- convert_to_phyloseq(sequencing_object = sequencing_object, treefile = tree_file, taxonomy = taxonomy_file)
  message('Converted to phyloseq object...')
  
  message('Ordinating Relative Abundance data...')
  # set seed for nmds ordination
  set.seed(12345)
  
  relative_ordination <- phyloseq::ordinate(phyloseq_object$Relative, method = method, distance = distance)
  
  message('Ordinating Absolute Abundance data...')
  absolute_ordination <- phyloseq::ordinate(phyloseq_object$Absolute, method = method, distance = distance)
  
  # Revert seed to random
  set.seed(Sys.time())
  
  message('Generating output plot data...')
  
  if (method %in% c('NMDS', 'DCA')) {
    relative_ordination_points <- data.frame(vegan::scores(relative_ordination))
    relative_pct_explained <- NA
    absolute_ordination_points <- data.frame(vegan::scores(absolute_ordination))
    absolute_pct_explained <- NA
  } else if (method == 'PCoA') {
    relative_ordination_points <- data.frame(relative_ordination$vectors[,1:n_axes])
    relative_pct_explained <- relative_ordination$values$Relative_eig[1:n_axes]
    absolute_ordination_points <- data.frame(absolute_ordination$vectors[,1:n_axes])
    absolute_pct_explained <- absolute_ordination$values$Relative_eig[1:n_axes]
  }
  
  
  relative_plot_data <- relative_ordination_points %>% tibble::rownames_to_column(var = 'X.SampleID') %>% dplyr::left_join(sequencing_object$sample_metadata, var = 'X.SampleID')
  absolute_plot_data <- absolute_ordination_points %>% tibble::rownames_to_column(var = 'X.SampleID') %>% dplyr::left_join(sequencing_object$sample_metadata, var = 'X.SampleID')
  
  
  message('Finalizing output...')
  output <- list(seq_object = sequencing_object, phyloseq_object = phyloseq_object, relative_ord =  relative_ordination, absolute_ord = absolute_ordination, rel_plot_data = relative_plot_data, rel_pct_explained = relative_pct_explained, abs_plot_data = absolute_plot_data, abs_pct_explained = absolute_pct_explained)
  
  return(output)
}
