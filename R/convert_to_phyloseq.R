#' Function to convert sequencing object to phyloseq object with optional addition of phylogenetic tree and taxonomy data
#'
#' @param sequencing_object output of individual_scale function (will contian sample metadata)
#' @param treefile (optional) greengenes taxonomic tree file
#' @param taxonomy (optional) greengenes taxonomic information file
#'
#' @return phyloseq object with provided data
#' @export

convert_to_phyloseq <-
  function(sequencing_object,
           treefile = NULL,
           taxonomy = NULL) {
    # make biom tables in temporary folder
    output <-
      make_biom_tables(sequencing_object, output_type = 'phyloseq')
    sample_metadata <-
      phyloseq::sample_data(tibble::column_to_rownames(sequencing_object$sample_metadata, 'X.SampleID'))
    
    # import taxonomy
    if (!is.null(taxonomy)) {
      taxonomy_object <- taxonomy_parser(taxonomy)
      pruned_taxonomy <-
        phyloseq::prune_taxa(phyloseq::taxa_names(output[['Relative']]), taxonomy_object)
    } else {
      pruned_taxonomy <- NULL
    }
    if (!is.null(treefile)) {
      tree_object <- phyloseq::read_tree_greengenes(treefile)
      pruned_tree <-
        phyloseq::prune_taxa(phyloseq::taxa_names(output[['Relative']]), tree_object)
    } else {
      pruned_tree <- NULL
    }
    
    for (i in c("Relative", "Absolute")) {
      output[[i]] <-
        phyloseq::phyloseq(output[[i]], pruned_taxonomy, pruned_tree, sample_metadata)
    }
    
    return(output)
  }
