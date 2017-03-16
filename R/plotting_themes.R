#' Theme function for plotting with tilted axes
#' @param base_size  base text size
#' @param base_family base text family
#' @param bgcolor background color
#' @export
#'

EJC_theme_tilted <- function(base_size = 12, base_family = "sans", bgcolor = "default") {
    bgcol <- NULL
    ret <- ggplot2::theme(
		rect = ggplot2::element_rect(fill = bgcol, linetype = 0, colour = NA), 
		text = ggplot2::element_text(size = base_size, family = base_family, color = "black"), 
		
		title = ggplot2::element_text(size = 24, hjust = 0.5), 
		
		axis.title.x = ggplot2::element_text(size = 18,hjust = 0.5), 
		axis.line = ggplot2::element_line(color = "black"), 
		axis.text.x = ggplot2::element_text(size = 18, angle = 45, hjust = 1, color = "black"), 
		axis.text.y = ggplot2::element_text(size = 18, color = "black"), 
		axis.title.y = ggplot2::element_text(size = 18,hjust = 0.5), 
		
		panel.grid.major.y = ggplot2::element_blank(), 
		panel.grid.minor.y = ggplot2::element_blank(), 
        panel.grid.major.x = ggplot2::element_blank(), 
		panel.grid.minor.x = ggplot2::element_blank(), 
		panel.border = ggplot2::element_blank(), 
        panel.background = ggplot2::element_rect(fill = NA), 
		
		legend.position = "bottom", 
		legend.background = ggplot2::element_rect(fill = "gray95", colour = "black"), 
		legend.key = ggplot2::element_rect(fill = NA), 
		legend.key.size = ggplot2::unit(0.5, "cm"), 
        legend.title.align = 0.5, 
		legend.title = ggplot2::element_text(size = 14, face = "bold"),
		strip.background = element_rect(colour = NA, fill = NA))
    ret
}

#' Theme function for plotting
#' @param base_size  base text size
#' @param base_family base text family
#' @param bgcolor background color
#' @export
#'

EJC_theme <- function(base_size = 12, base_family = "sans", bgcolor = "default") {
    bgcol <- NULL
    ret <- ggplot2::theme(
		rect = ggplot2::element_rect(fill = bgcol, linetype = 0, colour = NA), 
		text = ggplot2::element_text(size = base_size, family = base_family, color = "black"), 
		
		title = ggplot2::element_text(size = 24, hjust = 0.5), 
		
		axis.title.x = ggplot2::element_text(size = 18, hjust = 0.5), 
		axis.line = ggplot2::element_line(color = "black"), 
		axis.text.x = ggplot2::element_text(size = 18, color = "black"), 
		axis.text.y = ggplot2::element_text(size = 18, color = "black"), 
		axis.title.y = ggplot2::element_text(size = 18,hjust = 0.5), 
		
		panel.grid.major.y = ggplot2::element_blank(), 
		panel.grid.minor.y = ggplot2::element_blank(), 
        panel.grid.major.x = ggplot2::element_blank(), 
		panel.grid.minor.x = ggplot2::element_blank(), 
		panel.border = ggplot2::element_blank(), 
        panel.background = ggplot2::element_rect(fill = NA), 
		
		legend.position = "bottom", 
		legend.background = ggplot2::element_rect(fill = "gray95", colour = "black"), 
		legend.key = ggplot2::element_rect(fill = NA), 
		legend.key.size = ggplot2::unit(0.5, "cm"), 
        legend.title.align = 0.5, 
		legend.title = ggplot2::element_text(size = 14, face = "bold"),
		strip.background = element_rect(colour = NA, fill = NA))
    ret
}

#' Theme function for plotting
#' @param base_size  base text size
#' @param base_family base text family
#' @param bgcolor background color
#' @export
#'

paper_theme <-
  function(base_size = 8,
           base_family = "sans",
           bgcolor = "default") {
    bgcol <- NULL
    ret <-
      ggplot2::theme(
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
        title = ggplot2::element_text(size = 10, hjust = 0.5),
        axis.title.x = ggplot2::element_text(size = 8,
                                             hjust = 0.5),
        axis.line = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_text(
          size = 8,
          hjust = 0.5,
          color = "black"
        ),
        axis.text.y = ggplot2::element_text(size = 8, color = "black"),
        axis.title.y = ggplot2::element_text(size = 8,
                                             hjust = 0.5),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        legend.position = "right",
        legend.background = ggplot2::element_rect(fill = NULL,
                                                  colour = "black", linetype = 'solid', size = 0.5),
        legend.key = ggplot2::element_rect(fill = NA),
        legend.key.size = ggplot2::unit(0.25, "cm"),
        legend.title.align = 0.5,
        legend.margin = ggplot2::margin(1.5,1.5,1.5,1.5),
        legend.title = ggplot2::element_text(size = 8, face = "bold"),
		strip.background = element_rect(colour = NA, fill = NA)
      )
    return(ret)
  }

#' Theme function for plotting with tilted axes
#' @param base_size  base text size
#' @param base_family base text family
#' @param bgcolor background color
#' @export
#'

paper_theme_tilted <-
  function(base_size = 8,
           base_family = "sans",
           bgcolor = "default") {
    bgcol <- NULL
    ret <-
      ggplot2::theme(
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
        title = ggplot2::element_text(size = 10, hjust = 0.5),
        axis.title.x = ggplot2::element_text(size = 8,
                                             hjust = 0.5),
        axis.line = ggplot2::element_line(color = "black"),
        axis.text.x = ggplot2::element_text(
          size = 8,
          hjust = 1,
          angle = 45,
          color = "black"
        ),
        axis.text.y = ggplot2::element_text(size = 8, color = "black"),
        axis.title.y = ggplot2::element_text(size = 8,
                                             hjust = 0.5),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = NA),
        legend.position = "right",
        legend.background = ggplot2::element_rect(fill = NULL,
                                                  colour = "black", linetype = 'solid', size = 0.5),
        legend.key = ggplot2::element_rect(fill = NA),
        legend.key.size = ggplot2::unit(0.25, "cm"),
        legend.title.align = 0.5,
        legend.margin = ggplot2::margin(1.5,1.5,1.5,1.5),
        legend.title = ggplot2::element_text(size = 8, face = "bold"),
		strip.background = element_rect(colour = NA, fill = NA)
      )
    return(ret)
  }
