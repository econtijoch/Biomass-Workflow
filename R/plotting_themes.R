#' Theme function for plotting with tilted axes
#' @param base_size  base text size
#' @param base_family base text family
#' @param bgcolor background color
#' @export
#'

EJC_theme_tilted <-
  function(base_size = 12,
           base_family = "sans",
           bgcolor = "default") {
    bgcol <- NULL
    ret <- ggplot2::theme(
      rect = ggplot2::element_rect(
        fill = bgcol,
        linetype = 0,
        colour = NA
      ),
      text = ggplot2::element_text(
        size = base_size,
        family = base_family,
        color = "black"
      ),

      title = ggplot2::element_text(size = 24, hjust = 0.5),

      axis.title.x = ggplot2::element_text(size = 18, hjust = 0.5),
      axis.line = ggplot2::element_line(color = "black"),
      axis.text.x = ggplot2::element_text(
        size = 18,
        angle = 45,
        hjust = 1,
        color = "black"
      ),
      axis.text.y = ggplot2::element_text(size = 18, color = "black"),
      axis.title.y = ggplot2::element_text(size = 18, hjust = 0.5, angle = 90),

      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA),
	  panel.ontop = FALSE,

      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "gray95", colour = "black"),
      legend.key = ggplot2::element_rect(fill = NA),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      legend.title.align = 0.5,
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      strip.background = ggplot2::element_rect(colour = NA, fill = NA),
      strip.placement = 'outside',
      strip.text = ggplot2::element_text(face = 'bold'),
	  plot.tag.position = "topleft",
    )
    ret
  }

#' Theme function for plotting
#' @param base_size  base text size
#' @param base_family base text family
#' @param bgcolor background color
#' @export
#'

EJC_theme <-
  function(base_size = 12,
           base_family = "sans",
           bgcolor = "default") {
    bgcol <- NULL
    ret <- ggplot2::theme(
      rect = ggplot2::element_rect(
        fill = bgcol,
        linetype = 0,
        colour = NA
      ),
      text = ggplot2::element_text(
        size = base_size,
        family = base_family,
        color = "black"
      ),

      title = ggplot2::element_text(size = 24, hjust = 0.5),

      axis.title.x = ggplot2::element_text(size = 18, hjust = 0.5),
      axis.line = ggplot2::element_line(color = "black"),
      axis.text.x = ggplot2::element_text(size = 18, color = "black"),
      axis.text.y = ggplot2::element_text(size = 18, color = "black"),
      axis.title.y = ggplot2::element_text(size = 18, hjust = 0.5, angle = 90),

      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA),
  	  panel.ontop = FALSE,

      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "gray95", colour = "black"),
      legend.key = ggplot2::element_rect(fill = NA),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      legend.title.align = 0.5,
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      strip.background = ggplot2::element_rect(colour = NA, fill = NA),
      strip.placement = 'outside',
      strip.text = ggplot2::element_text(face = 'bold'),
      plot.tag.position = "topleft",
    )
    ret
  }

#' Theme function for plotting paper-ready figures
#' @export
#'

paper_theme <-
  function() {
    ret <-
      ggplot2::theme(complete = TRUE,
                     validate = TRUE,
                     rect = ggplot2::element_rect(fill = NA, linetype = 0, colour = NA, size = 0),
                     text = ggplot2::element_text(size = 6, family = "ArialMT", color = "black", face = 'plain', hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), debug = FALSE),
                     title = ggplot2::element_text(size = 7, hjust = 0.5, face = 'bold'),
                     axis.line = ggplot2::element_line(color = "black"),
                     axis.ticks = ggplot2::element_line(color = "black"),
                     axis.ticks.length = ggplot2::unit(2, "pt"),
                     axis.text = ggplot2::element_text(size = 6),
                     axis.title.y = ggplot2::element_text(angle = 90),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
					 panel.ontop = FALSE,
                     legend.position = "right",
                     legend.background = ggplot2::element_rect(colour = "black", linetype = 'solid', size = 0.5),
                     legend.key.size = ggplot2::unit(0.25, "cm"),
                     legend.title.align = 0.5,
                     legend.margin = ggplot2::margin(1.5, 1.5, 1.5, 1.5),
                     strip.background = ggplot2::element_rect(colour = NA, fill = NA),
                     strip.placement = 'outside',
                     strip.text = ggplot2::element_text(face = 'bold'),
					 plot.tag.position = "topleft",
      )
    return(ret)
  }

#' Theme function for plotting paper-ready figures with tilted axes
#' @export
#'

paper_theme_tilted <-
  function() {
    ret <-
      ggplot2::theme(complete = TRUE,
                     validate = TRUE,
                     rect = ggplot2::element_blank(),
                     text = ggplot2::element_text(size = 6, family = "ArialMT", color = "black", face = 'plain', hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), debug = FALSE),
                     title = ggplot2::element_text(size = 7, hjust = 0.5, face = 'bold'),
                     axis.line = ggplot2::element_line(color = "black"),
                     axis.ticks = ggplot2::element_line(color = "black"),
                     axis.text = ggplot2::element_text(size = 6),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                     axis.title.y = ggplot2::element_text(angle = 90),
                     axis.ticks.length = ggplot2::unit(2, "pt"),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
					 panel.ontop = FALSE,
                     legend.position = "right",
                     legend.background = ggplot2::element_rect(colour = "black", linetype = 'solid', size = 0.5),
                     legend.key.size = ggplot2::unit(0.25, "cm"),
                     legend.title.align = 0.5,
                     legend.margin = ggplot2::margin(1.5, 1.5, 1.5, 1.5),
                     strip.background = ggplot2::element_rect(colour = NA, fill = NA),
                     strip.placement = 'outside',
                     strip.text = ggplot2::element_text(face = 'bold'),
					 plot.tag.position = "topleft"
      )
    return(ret)
  }


#' Theme function for plotting presentation-ready figures
#' @export
#'

slides_theme <-
  function() {
    ret <-
      ggplot2::theme(complete = TRUE,
                     validate = TRUE,
                     rect = ggplot2::element_blank(),
                     text = ggplot2::element_text(size = 24, family = "ArialMT", color = "black", face = 'plain', hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), debug = FALSE),
                     title = ggplot2::element_text(size = 28, hjust = 0.5, face = 'bold'),
                     axis.line = ggplot2::element_line(color = "black", size = 2),
                     axis.ticks = ggplot2::element_line(color = "black", size = 2),
                     axis.text = ggplot2::element_text(size = 24),
                     axis.ticks.length = ggplot2::unit(4, "pt"),
                     axis.title.y = ggplot2::element_text(angle = 90),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
					 panel.ontop = FALSE,
                     legend.position = "right",
                     legend.background = ggplot2::element_rect(colour = "black", linetype = 'solid', size = 0.5),
                     legend.key.size = ggplot2::unit(0.25, "cm"),
                     legend.title.align = 0.5,
                     legend.margin = ggplot2::margin(1.5, 1.5, 1.5, 1.5),
                     strip.background = ggplot2::element_rect(colour = NA, fill = NA),
                     strip.placement = 'outside',
                     strip.text = ggplot2::element_text(face = 'bold'),
					 plot.tag.position = "topleft"
      )
    return(ret)
  }

#' Theme function for plotting presentation-ready figures with tilted axes
#' @export
#'

slides_theme_tilted <-
  function() {
    ret <-
      ggplot2::theme(complete = TRUE,
                     validate = TRUE,
                     rect = ggplot2::element_blank(),
                     text = ggplot2::element_text(size = 24, family = "ArialMT", color = "black", face = 'plain', hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), debug = FALSE),
                     title = ggplot2::element_text(size = 28, hjust = 0.5, face = 'bold'),
                     axis.line = ggplot2::element_line(color = "black", size = 2),
                     axis.ticks = ggplot2::element_line(color = "black", size = 2),
                     axis.text = ggplot2::element_text(size = 24),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                     axis.ticks.length = ggplot2::unit(4, "pt"),
                     axis.title.y = ggplot2::element_text(angle = 90),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
					 panel.ontop = FALSE,
                     legend.position = "right",
                     legend.background = ggplot2::element_rect(colour = "black", linetype = 'solid', size = 1),
                     legend.key.size = ggplot2::unit(0.5, "cm"),
                     legend.title.align = 0.5,
                     legend.margin = ggplot2::margin(1.5, 1.5, 1.5, 1.5),
                     strip.background = ggplot2::element_rect(colour = NA, fill = NA),
                     strip.placement = 'outside',
                     strip.text = ggplot2::element_text(face = 'bold'),
					 plot.tag.position = "topleft"
      )
    return(ret)
  }
