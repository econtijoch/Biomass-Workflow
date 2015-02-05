ParsePlateReaderFile <- function(plate_reader_file) {
    
  #Import file
  file <- read.csv(plate_reader_file)
  
  #Find start point
  for (i in 0:ncol(file)) {
      if (length((grep("Well", file[,i]))) > 0) {
        title_row <- grep("Well", file[,i])
        end_row <- grep("Well", file[,i]) + 96
        start_column <- i                                                 
    }
  }
  start_row = title_row + 1;

  # Parse table
  parsed_table <- file[start_row:end_row,start_column:ncol(file)]
   
  #Add column names
  colnames(parsed_table) <- make.names(lapply(file[title_row,start_column:ncol(file)], as.character))
  
  # Cast well names as characters
  parsed_table$Well <- as.character(parsed_table$Well)
  
  
  # Cast as numeric
  for (i in 2:ncol(parsed_table)) {
    parsed_table[,i] <- as.numeric(as.character(parsed_table[,i]))
  }
  
  #Remove NAs
  final_table <- subset(parsed_table, !is.na(parsed_table$Read.1))
  
  return(final_table)
  
}