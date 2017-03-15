#' Function to parse greengenes taxonomy file for use with phyloseq
#'
#' @param taxonomy_file string with path to taxonomy file (e.g. greengenes)
#'
#' @return taxonomy file suitable for use with phyloseq
#' @export

taxonomy_parser <- function(taxonomy_file) {
  phylogeny <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  taxonomy_raw <- readr::read_delim(taxonomy_file, delim = "\t", col_names = c("OTU_ID", "taxonomy"), col_types = c(readr::col_character(), readr::col_character()))
  taxonomy_table <- tibble::column_to_rownames(as.data.frame(taxonomy_raw), var = "OTU_ID")
  split_tax <- taxonomy_table %>% tidyr::separate(taxonomy, phylogeny, sep = "; .__", remove = T, fill = "right")
  split_tax$Kingdom <- gsub(pattern = "k__", replacement = "", x = split_tax$Kingdom)
  taxonomy_matrix <- as.matrix(split_tax)
  taxonomy_phyloseq_object <- phyloseq::tax_table(taxonomy_matrix)
  return(taxonomy_phyloseq_object)
}
