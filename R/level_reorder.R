#' Reorder factor levels
#'
#' @param vector vector to reorder
#' @param order order of reordered factor levels
#'
#' @return reoredered factor
#' @export
#'
level_reorder <- function(vector, order) {
  vector <- factor(vector, levels = order)
  return(vector)
}
