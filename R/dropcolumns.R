#' Simple function to remove columns that are only comprised of NA's
#' @param table table to remove NAs from
#' @return output table with no columns that are only NAs (trims columns)
#' @export
#'


dropcolumns <- function(table) {
    return(table[colSums(!is.na(table)) > 0])
}
