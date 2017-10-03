#' Function to read in mapping file and tweak format so that it is friendly with the rest of the package functinos
#' @param mapping_file string containing path to mapping file
#' @return Mapping information in a data frame
#' @export
#'

ParseMappingFile <- function(mapping_file) {
    
  # Import file, handle xls(x) vs csv files
  if (utils::tail(unlist(strsplit(mapping_file, "\\.")), n = 1) == "csv") {
    file <- readr::read_csv(mapping_file)
  } else if (utils::tail(unlist(strsplit(mapping_file, "\\.")), n = 1) == "xls" | utils::tail(unlist(strsplit(mapping_file, 
                                                                                                                   "\\.")), n = 1) == "xlsx") {
    file <- readxl::read_excel(mapping_file)
  }
  
    # Cast well names as characters
    table <- file %>% dplyr::mutate(ReaderWell = as.character(ReaderWell))
    
    return(table)
}
