require(ggplot2)
require(cowplot)

StandardAnalysis <- function(standards_plate_reader_csv_file, standards_mapping_csv_file, exp_id, ...) {
	
	args <- list(...)
	num_reads = 1
	
	if (!is.null(args$num_reads)) {
		num_reads = args$num_reads
	}
  
  # Read in raw data file from the .csv output of the plate reader. This will produce a data frame with well and read information for the plate.
  	rawdata <- PlateParser(standards_plate_reader_csv_file, num_reads)

# Read barcode ID's from a file containing the label information

mapping <- ParseMappingFile(standards_mapping_csv_file)


# Merge data with mapping file, label data appropriately
data <- merge(rawdata$table, mapping, by = "ReaderWell")

data <- subset(data, !is.na(data$BarcodeID))
data <- subset(data, data$BarcodeID != "")

rownames(data) <- data$BarcodeID

# Add average fluorescence column to data
read_start = 2
read_end = rawdata$num_reads +1

data$fluor_av <- apply(data[read_start:read_end], 1, mean)

standard_table <- split(data, data$Type)$Standard
standard_table <- standard_table[order(standard_table$BarcodeID),]

s_y <- standard_table$SampleMass
s_x <- standard_table$fluor_av

standards <- data.frame(s_x, s_y)

colnames(standards) <- c("x", "y")
standard_curve <- lm(standards$y~standards$x)

scale_x <- standard_curve$coefficients[2]
intercept <- standard_curve$coefficients[1]
rsquared <- summary(standard_curve)$r.squared

# Make Plot of Standard Curve

usr <- par("usr")
standards_info <- paste(paste("Line of best fit: Y = ", round(scale_x,5), "* X ", round(intercept, 5)), paste("R^2 = ", round (rsquared,5)), sep = "\n")

name <- paste(exp_id, "Standard Curve.pdf")
standards_plot <- ggplot(standards, aes_string(x = 's_x', y = 's_y')) + geom_point(size = 4) + labs(x = "Fluorescence Measurement", y = "ug DNA in Standard", main = "Standard Curve") + geom_smooth(method = "lm", se = FALSE) + annotate("text", x = 0, y = 1000, hjust = 0, label = standards_info) + EJC_theme() + theme(axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, color = 'black')) 

save_plot(name, standards_plot, base_height = 6, base_width = 6)


print("Standards Information:")
print(paste("Line of best fit: Y = ", round(scale_x,5), "* X ", round(intercept, 5)))
print(paste("R^2 = ", round (rsquared,5)))

output_list <- list("table" = standard_table, "scale_x" = scale_x, "intercept" = intercept)

return(output_list)

}