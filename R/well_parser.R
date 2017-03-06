#' Function to toggle between a well ID and a well location number (useful for use with the Beckmann robot)
#' @param well well ID (e.g  A01 or F05)
#' @param size size of plate (96 or 384)
#' @return A well number (e.g. 1 or 65)
#' @export
#'

well_parser <- function(well, size = 96) {
  row <- substring(well, 1, 1)
  column <- as.numeric(substring(well, 2, 3))
  num_row <- grep(pattern = row, LETTERS)
  if (size == 96) {
    output <- 12 * (num_row - 1) + column
  } else if (size == 384) {
    output <- 24 * (num_row - 1) + column
  }
  return(output)
}



