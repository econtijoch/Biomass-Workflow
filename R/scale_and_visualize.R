#' Function to perform individual_scale function AND vizualization functon
#'
#' @param biom_file .biom table in rhdf5 format. It is wise to use a pre-filtered table (for example, de-duplicated)
#' @param metadata_file the mapping file used in QIIME to split your sequencing library (must include microbial biomass data)
#' @param taxonomy_file a file containing taxonomy (e.g. output from assign_taxonomy.py in QIIME, or greengenes taxonomy)
#' @param filter OPTIONAL: minimum relative abundance to filter from data
#' @return a list containing OTU tables with and without taxonomy (at each given depth), both scaled and unscaled (the output is a list of lists). 
#' @export
#'

scale_and_visualize <- function(biom_file, metadata_file, taxonomy_file, filter = 0) {
  output <- BiomassWorkflow::individual_scale(biom_file = biom_file, metadata_file = metadata_file, taxonomy_file = taxonomy_file, filter = filter)
  BiomassWorkflow::sequencing_data_viz(output)
  return(output)
}
