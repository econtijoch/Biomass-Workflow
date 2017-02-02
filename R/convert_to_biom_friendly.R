## function to write biom-ready otu_table
##
##
#' Function to convert scaled/unscaled output of individual_scale to biom-friendly format
#'
#' @param table table from scaled/unscaled output of individual_scale
#' @param output_name name for output file (written)
#' @param scale_factor default = 1e6 -value to multiply all smaples by to generate "reads" that are scaled appropriately)
#'
#' @return output table
#' @export
#'

convert_to_biom_friendly <- function(table, output_name, scale_factor = 1e6) {
   pre_processed_table <- table %>% dplyr::select(X.SampleID, dplyr::starts_with("OTU_")) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column("#OTU ID")

  colnames(pre_processed_table) <- c("#OTU ID",t(pre_processed_table)[,1][-1])
  cleaned_table <- pre_processed_table[-1, ]

  cleaned_table$`#OTU ID` <- unlist(strsplit(cleaned_table[,1], "_"))[c(F, T)]
  rownames(cleaned_table) <- NULL
  numeric_matrix <- as.matrix(tibble::column_to_rownames(cleaned_table, "#OTU ID"))
  class(numeric_matrix) <- 'numeric'
  integer_matrix_scaled <- numeric_matrix*1e6
  class(integer_matrix_scaled) <- 'integer'

  output_table <- as.data.frame(integer_matrix_scaled) %>% tibble::rownames_to_column("#OTU ID")


  write("# Constructed from BiomassWorkflow R Package", file = output_name)
  readr::write_delim(x = output_table, path = output_name, delim = '\t', col_names = T, append = T)
  return(output_table)
}

