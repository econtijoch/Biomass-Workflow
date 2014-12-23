StandardAnalysis <- function(standards_plate_reader_csv_file, standards_mapping_csv_file, exp_id, BR_or_HS) {
  
  # Read in raw data file from the .csv output of the plate reader. This will produce a data frame with well and read information for the plate.
rawdata <- na.omit(read.csv(file = standards_plate_reader_csv_file , sep = ",", header = TRUE, skip = 9, nrows = 96, skipNul = FALSE, colClasses = c("NULL", "character", rep("numeric", 10))))

# Read barcode ID's from a file containing the label information

mapping <- na.omit(read.csv(file = standards_mapping_csv_file, sep = ",", fill = TRUE, header = TRUE, na.strings = "", colClasses = c(rep("character", 8), "numeric")))


# Merge data with mapping file, label data appropriately
data <- merge(rawdata, mapping, by = "Well")

rownames(data) <- data$BarcodeID

# Add average fluorescence column to data
data$fluor_av <- apply(data[2:11], 1, mean)

standard_table <- split(data, data$Type)$Standard
standard_table <- standard_table[order(standard_table$BarcodeID),]

exp_data <- split(data, data$Type)$Experiment

# Create the standards and the standard curve
if(BR_or_HS == "HS") {
	s_y <- c(0,5,10,20,40,60,80,100)
	} else { 
		s_y <- c(0,50,100,200,400,600,800,1000)
	}

s_x <- standard_table$fluor_av

standards <- data.frame(s_x,s_y)

colnames(standards) <- c("x", "y")
standard_curve <- lm(standards$y~standards$x)

scale_x <- standard_curve$coefficients[2]
intercept <- standard_curve$coefficients[1]
rsquared <- summary(standard_curve)$r.squared

# Make Plot of Standard Curve

name <- paste(exp_id, "Standard Curve.png")
png(name)
plot(standards, xlab = "Measurement", ylab = "Standard", main = "Standard Curve")
abline(standard_curve)
text(usr[ 1 ], usr[ 4 ], "left top", adj = c( 0, 1 ), paste("Y =", round(scale_x, 5), "* X ", round(intercept, 5), "\nR^2 = ", round(rsquared, 5)))
dev.off()

return(standard_table)

}