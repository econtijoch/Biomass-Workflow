#' ggname
#' Helper function taken from ggplot2 -- not exported, need to re-create
#' @param prefix prefix
#' @param grob grob
#'
#' @return ggname
#'
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

StatMeanSEM <- ggplot2::ggproto(
  "StatMeanSEM",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  
  
  setup_params = function(data, params) {
    params$width <- if (!is.null(params$width)) {
      params$width
    } else {
      ggplot2::resolution(data$x) * 0.75
    }
    
    if (is.double(data$x) &&
        !data$group[1L] != -1L && any(data$x != data$x[1L])) {
      warning("Continuous x aesthetic -- did you forget aes(group=...)?",
              call. = FALSE)
    }
    
    params
  },
  
  compute_group = function(data,
                           scales,
                           width = NULL,
                           na.rm = FALSE) {
    stats <-
      data %>% dplyr::group_by(x) %>% dplyr::summarize(
        ymean = mean(y),
        ymax = mean(y) + sd(y) / sqrt(n()),
        ymin = mean(y) - sd(y) / sqrt(n())
      )
    n <- sum(!is.na(data$y))
    
    
    
    if (length(unique(data$x)) > 1)
      width <-
      diff(range(data$x)) * 0.9
    
    df <- as.data.frame(stats)
    
    df$x <-
      if (is.factor(data$x))
        data$x[1]
    else
      mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df
  }
)


#' Layer function for stat
#'
#' @param mapping mapping
#' @param data dadta
#' @param geom geom
#' @param position position
#' @param ... ... 
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#'
#' @return plot layer
#' @export
#'

stat_mean_sem <- function(mapping = NULL,
                          data = NULL,
                          geom = "mean_sem",
                          position = "dodge",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMeanSEM,
    geom = GeomMeanSEM,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}

## Geom for mean + SEM layer

GeomMeanSEM <- ggplot2::ggproto(
  "GeomMeanSEM",
  ggplot2::Geom,
  setup_data = function(data, params) {
    data$width <- if (!is.null(data$width)) {
      data$width
    } else if (!is.null(params$width)) {
      params$width
    } else {
      ggplot2::resolution(data$x, FALSE) * 0.9
    }
    
    data$xmin <-
      data$x - data$width / 6
    data$xmax <-
      data$x + data$width / 6
    data$xleft <-
      data$x - data$width / 3
    data$xright <-
      data$x + data$width / 3
    data
    
  },
  
  draw_group = function(data, panel_scales, coord) {
    common <- data.frame(
      colour = 'black',
      size = data$size,
      linetype = data$linetype,
      fill = data$fill,
      group = data$group,
      alpha = data$alpha,
      stringsAsFactors = FALSE
    )
    
    vertical <- data.frame(
      x = data$x,
      xend = data$x,
      y = data$ymin,
      yend = data$ymax,
      common,
      stringsAsFactors = FALSE
    )
    
    top <- data.frame(
      x = data$xmin,
      xend = data$xmax,
      y = data$ymax,
      yend = data$ymax,
      common,
      stringsAsFactors = FALSE
    )
    
    bottom <- data.frame(
      x = data$xmin,
      xend = data$xmax,
      y = data$ymin,
      yend = data$ymin,
      common,
      stringsAsFactors = FALSE
    )
    
    middle <- data.frame(
      x = data$xleft,
      xend = data$xright,
      y = data$ymean,
      yend = data$ymean,
      common,
      stringsAsFactors = FALSE
    )
    
    
    ggname(
      "geom_mean_sem",
      grid::grobTree(
        ggplot2::GeomSegment$draw_panel(vertical, panel_scales, coord),
        ggplot2::GeomSegment$draw_panel(top, panel_scales, coord),
        ggplot2::GeomSegment$draw_panel(bottom, panel_scales, coord),
        ggplot2::GeomSegment$draw_panel(middle, panel_scales, coord)
      )
    )
  },
  
  draw_key = ggplot2::draw_key_boxplot,
  
  default_aes = ggplot2::aes(
    weight = 1,
    colour = "black",
    size = 1,
    linetype = "solid",
    alpha = NA,
    fill = NA
  ),
  
  required_aes = c("x", "ymean", "ymax", "ymin")
)


#' New layer function that plots both the summary statistics AND the quasirandom plot from ggbeeswarm
#'
#' @param mapping mapping
#' @param data data
#' @param position position 
#' @param ... ...
#' @param point.size point.size
#' @param point.color point.color
#' @param line.size line.size
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param width width
#' @param varwidth varwidth
#' @param bandwidth bandwidth
#' @param nbins nbins
#' @param method method
#' @param groupOnX grouponX
#' @param dodge.width dodge.width
#'
#' @return plot layer
#' @export
#'
geom_mean_sem <- function(mapping = NULL,
                          data = NULL,
                          position = "identity",
                          ...,
                          point.size = 1,
						              point.color = 'black',
                          line.size = 1,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          width = 0.2,
                          varwidth = FALSE,
                          bandwidth = .5,
                          nbins = NULL,
                          method = 'quasirandom',
                          groupOnX = NULL,
                          dodge.width = 0) {
  quasi <-
    ggbeeswarm::position_quasirandom(
      width = width,
      varwidth = varwidth,
      bandwidth = bandwidth,
      nbins = nbins,
      method = method,
      groupOnX = groupOnX,
      dodge.width = dodge.width
    )
  
  list(
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = 'identity',
      geom = ggplot2::GeomPoint,
      position = quasi,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(size = point.size,
                    na.rm = na.rm,
					color = point.color,
                    ...)
    ),
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatMeanSEM,
      geom = GeomMeanSEM,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    size = line.size,
                    ...)
    )
  )
}
