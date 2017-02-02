#' quick conversion to percent
#'
#' @param x number to convrt
#' @param digits number of digits
#' @param format format
#' @param ... ...
#'
#' @return percent formatted
#' @export
#'
percent <- function(x,
                    digits = 2,
                    format = "f",
                    ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
