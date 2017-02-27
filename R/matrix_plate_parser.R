#' Matrix plate parser
#'
#' @param matrix_barcode_plate_scan file from matrix rack scanner
#'
#' @return table of tubes
#' @export
#'

matrix_plate_parser <- function(matrix_barcode_plate_scan) {
  # Read in order file if .csv
  if (utils::tail(unlist(strsplit(matrix_barcode_plate_scan, "\\.")), n = 1) == "csv") {
    tube_order <-
      readr::read_csv(
        matrix_barcode_plate_scan,
        col_names = c("SampleWell", "Barcode"),
        col_types = list(readr::col_character(), readr::col_character())
      )
  } else if (utils::tail(unlist(strsplit(matrix_barcode_plate_scan, "\\.")), n = 1) == "xls" |
             utils::tail(unlist(strsplit(matrix_barcode_plate_scan,
                                         "\\.")), n = 1) == "xlsx") {
    # Read in order file if .xls , .xlsx
    order_file <-
      XLConnect::readWorksheet(
        object = XLConnect::loadWorkbook(matrix_barcode_plate_scan),
        sheet = 1,
        header = F
      )
    tube_order <-
      data.frame(
        SampleWell = NA,
        TubeBarcode = NA,
        stringsAsFactors = FALSE
      )
    
    # Parse plate layout to table
    rows <- LETTERS[1:8]
    cols <- sprintf(1:12, fmt = '%02.0f')
    
    rows_length <- length(rows)
    cols_length <- length(cols)
    
    wells <- ""
    for (r in 1:rows_length) {
      for (c in 1:cols_length) {
        index <- (r - 1) * 12 + c
        index_name <- paste(rows[r], cols[c], sep = "")
        wells <- c(wells, index_name)
        tube_order[index, "SampleWell"] <-
          as.character(index_name)
        if (nchar(as.character(order_file[r, c])) == 9) {
          tube_order[index, "TubeBarcode"] <-
            paste0("0", as.character(order_file[r, c]))
        } else {
          tube_order[index, "TubeBarcode"] <-
            as.character(order_file[r, c])
        }
        
        
      }
    }
  }
  return(tube_order)
}