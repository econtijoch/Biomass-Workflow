#' Function to read in mapping file and tweak format so that it is friendly with the rest of the package functinos
#' @param mapping_file string containing path to mapping file
#' @return Mapping information in a data frame
#' @export
#'

ParseMappingFile <- function(mapping_file) {
    
    # Import file
    table <- utils::read.csv(file = mapping_file, row.names = NULL)
    
    # Cast well names as characters
    table$ReaderWell <- as.character(table$ReaderWell)
    
    return(table)
}
