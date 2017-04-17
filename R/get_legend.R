#' Create a stand-alone legend for plotting
#'
#' @param ggplot_item ggplot item
#'
#' @return legend
#' @export
#' 
get_legend <- function(ggplot_item) {
  grobs <- ggplot2::ggplotGrob(ggplot_item + ggplot2::theme(legend.position = 'right'))$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == 'guide-box')]]
  return(legend)
}