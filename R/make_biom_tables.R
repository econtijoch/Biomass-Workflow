#' Make biom tables (relative abundance and absolute abundance) from a sequencing object (individual_scale output)
#'
#' @param sequencing_object output from individual_scale that contains microbial density-scaled sequencing data
#' @param output_type 'biom' or 'phyloseq', to indicate which format to generate the resulant OTU tables in within R
#' @param output_prefix (optional) prefix to give otu tables. By providing this, you will write .biom otu tables to your working directory or specified output directory
#' @param output_directory (optional) specify an output directory to write otu tables in .biom format to (must provide output_prefix)
#'
#' @return a list of otu tables (relative and absolute) in the specified format. Additionally, if specified, .biom files will be written
#' @export

make_biom_tables <-
  function(sequencing_object,
           output_type = 'biom',
           output_prefix = NULL,
           output_directory = getwd()) {
    map <- sequencing_object$sample_metadata
    rownames(map) <- map$X.SampleID
    
    biom_tables <- list()
    
    
    for (type in c("Relative", "Absolute")) {
      if (type == "Relative") {
        table <- sequencing_object$relative_otus
      } else {
        table <- sequencing_object$scaled_otus
      }
      pre_processed_table <-
        table %>% dplyr::select(X.SampleID, dplyr::starts_with("OTU_")) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column("#OTU ID")
      
      colnames(pre_processed_table) <-
        c("#OTU ID", t(pre_processed_table)[, 1][-1])
      cleaned_table <- pre_processed_table[-1,]
      
      otus <- unlist(strsplit(cleaned_table[, 1], "_"))[c(F, T)]
      cleaned_table$`#OTU ID` <- otus
      rownames(cleaned_table) <- NULL
      numeric_matrix <-
        as.matrix(tibble::column_to_rownames(cleaned_table, "#OTU ID"))
      class(numeric_matrix) <- 'numeric'
      integer_matrix_scaled <- numeric_matrix * 1e6
      class(integer_matrix_scaled) <- 'integer'
      
      
      if (output_type == 'biom') {
        biom_tables[[type]] <-
          biom::make_biom(data = integer_matrix_scaled, sample_metadata = map)
        
        if (!is.null(output_prefix)) {
          output_name <-
            paste0(output_directory,
                   "/",
                   output_prefix,
                   "_",
                   type,
                   ".biom")
          biom::write_biom(biom_tables[[type]], output_name)
        } else {
          
        }
      } else if (output_type == 'phyloseq') {
        biom_tables[[type]] <-
          phyloseq::otu_table(object = integer_matrix_scaled, taxa_are_rows = T)
      }
    }
    return(biom_tables)
  }