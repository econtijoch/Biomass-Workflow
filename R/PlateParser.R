#' Function to read in mapping file and tweak format so that it is friendly with the rest of the package functinos
#' @param plate_reader_file Fluorescence data from the plate reader (can be in .csv or .xls(x) format -- if in excel file, only the FIRST sheet within the file will be read)
#' @param num_reads OPTIONAL: number of measurements that the plate reader took for each sample (default = 1)
#' @param shiny OPTIONAL: necessary for running with shiny app interface since filenames are not the same.
#' @param type OPTIONAL: necessary for running with shiny app, must specify file type
#' @return a list containing: 1) a data frame with the data from the plate reader file and 2) the number of measurements taken for each sample
#' @export
#'

PlateParser <- function(plate_reader_file, num_reads = 1, shiny = FALSE, type = NULL) {
    
    
	if (shiny == TRUE) {
		if (type == 'csv') {
			file <- utils::read.csv(plate_reader_file)
		} else if (type == 'excel') {
			file <- XLConnect::readWorksheet(object = XLConnect::loadWorkbook(plate_reader_file), sheet = 1)
		}
	} 

	# Import file, handle xls(x) vs csv files
    if (utils::tail(unlist(strsplit(plate_reader_file, "\\.")), n = 1) == "csv") {
        file <- utils::read.csv(plate_reader_file)
    } else if (utils::tail(unlist(strsplit(plate_reader_file, "\\.")), n = 1) == "xls" | utils::tail(unlist(strsplit(plate_reader_file, 
        "\\.")), n = 1) == "xlsx") {
        file <- XLConnect::readWorksheet(object = XLConnect::loadWorkbook(plate_reader_file), sheet = 1)
    }
    
    
    # Find start point
    for (i in 0:ncol(file)) {
        if (length((grep("Results", file[, i]))) > 0) {
            title_row <- grep("Results", file[, i])
            end_row <- title_row + num_reads * 9 + 2
            start_column <- i + 2
        }
    }
    start_row = title_row + num_reads + 3
    
    # Parse table
    parsed_table <- file[start_row:end_row, start_column:(ncol(file) - 1)]
    
    newtable <- data.frame(matrix(0, nrow = 96, ncol = num_reads))
    
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
            for (replicate in 1:num_reads) {
                raw_table_row_replicate_index <- (num_reads * r - num_reads) + replicate
                newtable[index, replicate] <- parsed_table[raw_table_row_replicate_index, c]
            }
            
        }
    }
    newtable_data <- data.frame(ReaderWell = wells[-1], newtable)
    
    
    # Add column names
    column_titles <- "ReaderWell"
    for (i in 1:num_reads) {
        column_titles <- c(column_titles, paste("Read.", i, sep = ""))
    }
    colnames(newtable_data) <- column_titles
    
    # Cast well names as characters
    newtable_data$ReaderWell <- as.character(newtable_data$ReaderWell)
    
    
    # Cast as numeric
    for (i in 2:ncol(newtable_data)) {
        newtable_data[, i] <- as.numeric(as.character(newtable_data[, i]))
    }
    
    # Remove NAs
    final_table <- subset(newtable_data, !is.na(newtable_data$Read.1))
    
    output_list <- list(table = final_table, num_reads = num_reads)
    
    return(output_list)
    
}
