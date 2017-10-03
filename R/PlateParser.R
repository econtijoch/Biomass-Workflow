#' Function to read in mapping file and tweak format so that it is friendly with the rest of the package functinos
#' @param plate_reader_file Fluorescence data from the plate reader (can be in .csv or .xls(x) format -- if in excel file, only the FIRST sheet within the file will be read)
#' @param size optional size of plate (96 or 384)
#' @param shiny OPTIONAL: necessary for running with shiny app interface since filenames are not the same.
#' @param type OPTIONAL: necessary for running with shiny app, must specify file type
#' @return a list containing: 1) a data frame with the data from the plate reader file and 2) the number of measurements taken for each sample
#' @export
#'

PlateParser <- function(plate_reader_file, shiny = FALSE, type = NULL, size = 96) {
    
    
	if (shiny == TRUE) {
		if (type == 'csv') {
			file <- readr::read_csv(plate_reader_file)
		} else if (type == 'excel') {
			file <- readxl::read_excel(plate_reader_file)
		}
	} else {
		# Import file, handle xls(x) vs csv files
		if (utils::tail(unlist(strsplit(plate_reader_file, "\\.")), n = 1) == "csv") {
		  file <- readr::read_csv(plate_reader_file)
    	} else if (utils::tail(unlist(strsplit(plate_reader_file, "\\.")), n = 1) == "xls" | utils::tail(unlist(strsplit(plate_reader_file, 
        	"\\.")), n = 1) == "xlsx") {
    	  file <- readxl::read_excel(plate_reader_file)
    	}
    
	}
    
    # Find start point
    for (i in 0:ncol(file)) {
        if (length((grep("Results", file[, i]))) > 0) {
            title_row <- grep("Results", file[, i])
            if (size == 96) {
              end_row <- title_row + 11
            } else {
              end_row <- title_row + 23
            }
            
            start_column <- i + 2
        }
    }
    start_row = title_row + 4
    
    # Parse table
    parsed_table <- file[start_row:end_row, start_column:(ncol(file) - 1)]
    
    newtable <- data.frame(ReaderWell = NA, Fluorescence = NA, stringsAsFactors = FALSE)
    
    if (size == 96) {
      rows <- LETTERS[1:8]
      cols <- sprintf(1:12, fmt = '%02.0f')
      
    } else if (size == 384) {
      rows <- LETTERS[1:16]
      cols <- sprintf(1:24, fmt = '%02.0f')
    } else {
      stop('Size must be 96 or 384')
    }
    
    rows_length <- length(rows)
    cols_length <- length(cols)
    
    for (r in 1:rows_length) {
        for (c in 1:cols_length) {
            index <- (r - 1) * cols_length + c
            index_name <- paste(rows[r], cols[c], sep = "")
            newtable[index, "ReaderWell"] <- as.character(index_name)
            newtable[index, "Fluorescence"] <- as.numeric(as.character(parsed_table[r, c]))
        }
    }
    
    # Remove NAs
    final_table <- subset(newtable, !is.na(newtable$Fluorescence))
    
    return(final_table)
    
}
