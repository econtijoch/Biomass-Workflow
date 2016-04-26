require(ggthemes)

EJC_theme <- function (base_size = 12, base_family = "sans", bgcolor = "default") {
  bgcol <- NULL
  ret <- theme(rect = element_rect(fill = bgcol, linetype = 0,
                                   colour = NA), text = element_text(size = base_size, family = base_family, color = 'black'), 
               title = element_text(size = 24, hjust = 0.5), axis.title.x = element_text(size = 18, hjust = 0.5),
               axis.line=element_line(color = 'black'), axis.text.x = element_text(size = 18, angle = 45, hjust =1, color = 'black'),  axis.text.y = element_text(size = 18, color = 'black'),
               axis.title.y = element_text(size = 18, hjust = 0.5), panel.grid.major.y = element_blank(), 
               panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), 
               panel.grid.minor.x = element_blank(), panel.border = element_blank(), 
               panel.background = element_rect(fill = NA), legend.position = "bottom", legend.background= element_rect(fill = 'gray95', colour = 'black'),
               legend.key = element_rect(fill = NA), legend.key.size = unit(0.5, 'cm'), legend.title.align = 0.5, legend.title = element_text(size = 14, face = 'bold'))
  ret
}
#legend.background= element_rect(fill = 'gray95', colour = 'black') ; panel.background = element_rect(fill = 'gray99'); panel.grid.major.y = element_line(color = "gray85"), 


EJC_colors <- rep(c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
            "#CC79A7", "#999999", "#E69F00", 'red', 'blue', 'magenta', 'purple', 
            'cyan', 'yellow', 'navyblue', 'tan', 'seagreen', 'green', 'goldenrod', 
            'antiquewhite3', 'darkolivegreen', 'brown', 'deeppink3'), 100)
