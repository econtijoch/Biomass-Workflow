ParseMappingFile <- function(mapping_file) {
  
  #Import file
  file <- read.csv(mapping_file)
 # mapping <- na.omit(read.csv(file = standards_mapping_csv_file, sep = ",", fill = TRUE, header = TRUE, na.strings = "", colClasses = c(rep("character", 8), "numeric")))
  
  # Cast well names as characters
  file$ReaderWell <- as.character(file$ReaderWell)
  
  return(file)
}