NewPlateParser <- function(plate_reader_file) {
  
  #Import file
  file <- read.csv(plate_reader_file)
  
  #Find start point
  for (i in 0:ncol(file)) {
    if (length((grep("Results", file[,i]))) > 0) {
      title_row <- grep("Results", file[,i])
      end_row <- grep("Results", file[,i]) + 29
      start_column <- i  + 2                                               
    }
  }
  start_row = title_row + 6;
  num_reads = 3
  
  # Parse table
  parsed_table <- file[start_row:end_row,start_column:(ncol(file)-1)]
  
  newtable <- data.frame(matrix(0, nrow = 96, ncol = 3))
  
  rows <- c('A', 'B','C','D','E','F','G','H')
  cols <- c('1','2','3','4','5','6','7','8','9','10','11','12')
  
  rows_length <- length(rows)
  cols_length <- length(cols)
  
  wells <- "Well"
  for (r in 1:rows_length) {
    for (c in 1:cols_length) {

      index <- (r-1)*12 + c
      index_name <- paste(rows[r],cols[c], sep = "")
      wells <- c(wells, index_name)
      for (replicate in 1:3) {
        raw_table_row_replicate_index <- (3*r - 3) + replicate
        newtable[index, replicate] <- parsed_table[raw_table_row_replicate_index, c]
      }
    
    }
  }
  newtable_data <- data.frame(Well = wells[-1], newtable)

  
  #Add column names
  colnames(newtable_data) <- c("Well", "Read.1", "Read.2", "Read.3")
  
  # Cast well names as characters
  newtable_data$Well <- as.character(newtable_data$Well)
  
  
  # Cast as numeric
  for (i in 2:ncol(newtable_data)) {
    newtable_data[,i] <- as.numeric(as.character(newtable_data[,i]))
  }
  
  #Remove NAs
  final_table <- subset(newtable_data, !is.na(newtable_data$Read.1))
  
  output_list <- list("table" = final_table, "num_reads" = num_reads)
  
  return(output_list)
  
}