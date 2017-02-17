#' Read Label Pairings
#'
#' @param label_barcode_pairings File that contains scanned barcodes and labels
#' @param col1_name optional name of first item scanned (i.e. tube barcode or label barcode)
#' @param col2_name optional name of second item scanned (i.e. label barcode or tube barcode)
#' @param save optional to save resulting table to file in current working directory
#'
#' @return table with pairings
#' @export

read_label_pairings <- function(label_barcode_pairings, col1_name = 'col1', col2_name = 'col2', save = F) {
  input <- stringr::str_split(readr::read_lines('~/Desktop/testing.txt'), pattern = '\t')[[1]]
  if (length(input) %% 2 != 0) {
    input <- input[-length(input)]
  }
  col1 <- input[c(T,F)]
  col2 <- input[c(F,T)]
  table <- data.frame(col1, col2)
  colnames(table) <- c(col1_name, col2_name)
  if (save) {
    readr::write_csv(x = table, path = './Label_barcode_pairing.csv')
  }
  return(table)
}
