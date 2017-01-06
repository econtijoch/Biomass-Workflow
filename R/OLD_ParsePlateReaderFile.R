#' Deprecated plate reader file parser
#' @param plate_reader_file plate reader file from old plate reader
#' @return list that includes data in table format and the number of measurements that the plate reader took per sample
#' @export
#'

Deprecated_ParsePlateReaderFile <- function(plate_reader_file) {
    
    # Import file
    file <- utils::read.csv(plate_reader_file)
    
    # Find start point
    for (i in 0:ncol(file)) {
        if (length((grep("Well", file[, i]))) > 0) {
            title_row <- grep("Well", file[, i])
            end_row <- grep("Well", file[, i]) + 96
            start_column <- i
        }
    }
    start_row = title_row + 1
    num_reads = ncol(file) - start_column
    
    # Parse table
    parsed_table <- file[start_row:end_row, start_column:ncol(file)]
    
    
    
    # Add column names
    colnames(parsed_table) <- make.names(lapply(file[title_row, start_column:ncol(file)], as.character))
    
    rows <- c("A", "B", "C", "D", "E", "F", "G", "H")
    cols <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    
    rows_length <- length(rows)
    cols_length <- length(cols)
    
    wells <- "Well"
    for (r in 1:rows_length) {
      for (c in 1:cols_length) {
        
        index <- (r - 1) * 12 + c
        index_name <- paste(rows[r], cols[c], sep = "")
        wells <- c(wells, index_name)
        
      }
    }
    
    # Cast well names as characters
    parsed_table$ReaderWell <- wells[-1]
    
    
    # Cast as numeric
    for (i in 2:ncol(parsed_table)) {
        parsed_table[, i] <- as.numeric(as.character(parsed_table[, i]))
    }
    
    # Remove NAs, rename column
    final_table <- dplyr::rename(subset(parsed_table, !is.na(parsed_table$Read.1)), SampleWell = Well)
    
    output_list <- list(table = final_table, num_reads = num_reads)
    
    return(output_list)
    
}
