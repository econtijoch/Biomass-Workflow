#' Function to process standards information and create line of best fit
#' @param standards_plate_reader_file the file path of the plate reader file for the standards (in .csv or .xls(x) format)
#' @param standards_mapping_csv_file mapping file for plate contianing standards (for wells with standards, Type must be 'Standard')
#' @param exp_id experiment id
#' @param num_reads OPTIONAL: number of measurements that the plate reader took per well (Default = 1)
#' @param shiny OPTIONAL: necessary for running with shiny app interface since filenames are not the same.
#' @param type OPTIONAL: necessary for running with shiny app, must specify file type
#' @param ... optional inputs
#' @return list containing a table of the standards, and the information for the standard curve
#' @export
#'

StandardAnalysis <- function(standards_plate_reader_file, standards_mapping_csv_file, exp_id, num_reads = 1, shiny = FALSE, type = NULL, ...) {
    
    
    # Read in raw data file from the .csv output of the plate reader. This will produce a data frame with well and read
    # information for the plate.
    rawdata <- PlateParser(standards_plate_reader_file, num_reads, shiny, type)
    
    # Read barcode ID's from a file containing the label information
    
    mapping <- ParseMappingFile(standards_mapping_csv_file)
    
    
    # Merge data with mapping file, label data appropriately
    data <- merge(rawdata$table, mapping, by = "ReaderWell")
    
    data <- subset(data, !is.na(data$BarcodeID))
    data <- subset(data, data$BarcodeID != "")
    
    rownames(data) <- data$BarcodeID
    
    # Add average fluorescence column to data
    read_start = 2
    read_end = rawdata$num_reads + 1
    
    data$fluor_av <- apply(data[read_start:read_end], 1, mean)
    
    standard_table <- split(data, data$Type)$Standard
    standard_table <- standard_table[order(standard_table$BarcodeID), ]
    
    s_y <- standard_table$SampleMass
    s_x <- standard_table$fluor_av
    
    standards <- data.frame(s_x, s_y)
    
    colnames(standards) <- c("x", "y")
    standard_curve <- stats::lm(standards$y ~ standards$x)
    
    scale_x <- standard_curve$coefficients[2]
    intercept <- standard_curve$coefficients[1]
    rsquared <- summary(standard_curve)$r.squared
    
    # Make Plot of Standard Curve
    standards_info <- paste(paste("Line of best fit: Y = ", round(scale_x, 5), "* X ", round(intercept, 5)), paste("R^2 = ", 
        round(rsquared, 5)), sep = "\n")
    
    name <- paste(exp_id, "Standard Curve.pdf")
    standards_plot <- ggplot2::ggplot(data = standards, ggplot2::aes_string(x = "s_x", y = "s_y")) + ggplot2::geom_point(size = 4) + 
        ggplot2::labs(x = "Fluorescence Measurement", y = "ug DNA in Standard", main = "Standard Curve") + ggplot2::geom_smooth(method = "lm", 
        se = FALSE) + ggplot2::annotate("text", x = 0, y = max(s_y), hjust = 0, label = standards_info) + EJC_theme() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 18, angle = 0, hjust = 0.5, color = "black"))
    
    if (shiny == FALSE) {
      cowplot::save_plot(filename = name, plot = standards_plot, base_height = 6, base_width = 6)
      
      cat("Standards Information:\n")
      cat(paste(paste("Line of best fit: Y = ", round(scale_x, 5), "* X ", round(intercept, 5)), "\n", sep = ""))
      cat(paste(paste("R^2 = ", round(rsquared, 5)), "\n", sep = ""))
    }
    
    
    output_list <- list(table = standard_table, scale_x = scale_x, intercept = intercept, plot = standards_plot)
    
    return(output_list)
    
}
