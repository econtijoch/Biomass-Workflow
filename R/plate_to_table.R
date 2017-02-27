#' Plate to table
#'
#' @param plate_layout file with plate layout (with headers)
#' @param size 96 wells or 384 (defaults to 96)
#'
#' @return samples in table format
#' @export
#'
plate_to_table <- function(plate_layout, size = 96) {
  if (utils::tail(unlist(strsplit(plate_layout, "\\.")), n = 1) == "csv") {
    plate_file <-
      readr::read_csv(
        plate_layout)
    
  } else if (utils::tail(unlist(strsplit(plate_layout, "\\.")), n = 1) == "xls" |
             utils::tail(unlist(strsplit(plate_layout,
                                         "\\.")), n = 1) == "xlsx") {
    plate_file <-
      XLConnect::readWorksheet(
        object = XLConnect::loadWorkbook(plate_layout),
        sheet = 1,
        header = F
      )
  }
  table_output <-
    data.frame(
      SampleWell = NA,
      Content = NA,
      stringsAsFactors = FALSE
    )
  
  # Parse plate layout to table
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
  
  wells <- ""
  for (r in 1:rows_length) {
    for (c in 1:cols_length) {
      index <- (r - 1) * length(cols) + c
      index_name <- paste(rows[r], cols[c], sep = "")
      wells <- c(wells, index_name)
      table_output[index, "SampleWell"] <-
        as.character(index_name)
      table_output[index, "Content"] <-
        as.character(plate_file[r + 1, c + 1])
      
      
    }
  }
  return(table_output)
}