PlotMaker <- function(analyzed_data, exp_id) {

plot <- ggplot(analyzed_data, aes(x = Condition, y = biomass_ratio, fill = Condition)) + 
  geom_boxplot() + guides(fill = FALSE) + stat_summary(fun.y=mean, geom="point", shape = 5, size = 4) + 
  ggtitle(exp_id) + ylab("Biomass (ug DNA per mg fecal pellet)") + theme(plot.title = element_text(face = "bold")) + ylim(-0.05,1.25)


png(paste(exp_id, "Plot.png"))
print(ggplot(analyzed_data, aes(x = Condition, y = biomass_ratio, fill = Condition)) + 
        geom_boxplot() + guides(fill = FALSE) + stat_summary(fun.y=mean, geom="point", shape = 5, size = 4) + 
        ggtitle(exp_id) + ylab("Biomass (ug DNA per mg fecal pellet)") + theme(plot.title = element_text(face = "bold")) + ylim(-0.05,1.25))
dev.off()

return(plot)
}