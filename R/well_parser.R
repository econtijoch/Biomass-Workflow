#' Function to toggle between a well ID and a well location number (useful for use with the Beckmann robot)
#' @param well well ID (e.g  A01 or F05)
#' @return A well number (e.g. 1 or 65)
#' @export
#'

well_parser <- function(well) {
    row <- substring(well, 1, 1)
    column <- as.numeric(substring(well, 2, 3))
    num_row <- switch(EXPR = row, A = 1, B = 2, C = 3, D = 4, E = 5, F = 6, G = 7, H = 8)
    output <- 12 * (num_row - 1) + column
    return(output)
}

