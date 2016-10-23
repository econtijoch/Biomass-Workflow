#' Deprecated function for Data Analysis. See DataAnalysis instead
#' @param plate_reader_csv_file plate reader file (old version)
#' @param mapping_csv_file mapping file
#' @param standards_plate_reader_csv_file plate reader file for plate containing standards (from old machine)
#' @param standards_mapping_csv_file mapping file for plate contianing standards (for wells with standards, Type must be 'Standard')
#' @param exp_id experiment id
#' @param ... other arguments
#' @return data table with biomass data calculated
#' @export
#'

Deprecated_DataAnalysis <- function(plate_reader_csv_file, mapping_csv_file, exp_id, standards_plate_reader_csv_file = plate_reader_csv_file, 
    standards_mapping_csv_file = mapping_csv_file, ...) {
    
    
    standard_analysis <- Deprecated_StandardAnalysis(standards_plate_reader_csv_file = standards_plate_reader_csv_file, 
        standards_mapping_csv_file = standards_mapping_csv_file, exp_id = exp_id)
    
    # Read in raw data file from the .csv output of the plate reader. This will produce a data frame with well and read
    # information for the plate.
    rawdata <- Deprecated_ParsePlateReaderFile(plate_reader_csv_file)
    
    # Parse Metadata from mapping file
    mapping <- ParseMappingFile(mapping_csv_file)
    
    
    # Merge data with mapping file, label data appropriately
    data <- merge(rawdata$table, mapping, by = "Well")
    
    data <- subset(data, !is.na(data$BarcodeID))
    
    rownames(data) <- data$BarcodeID
    
    # Add average fluorescence column to data
    read_start = 2
    read_end = rawdata$num_reads + 1
    
    data$fluor_av <- apply(data[read_start:read_end], 1, mean)
    
    exp_data <- split(data, data$Type)$Experiment
    
    scale_x <- standard_analysis$scale_x
    intercept <- standard_analysis$intercept
    
    # Begin working with the data
    exp_data$dna_concentration <- (exp_data[, "fluor_av"] * scale_x + intercept)/2
    
    # Biomass Analysis
    exp_data$total_dna <- exp_data[, "dna_concentration"] * 0.1
    exp_data$biomass_ratio <- exp_data$total_dna/exp_data$SampleMass
    exp_data$vol_needed_for_PCR <- 400/exp_data[, "dna_concentration"]
    exp_data$water_volume_up_PCR <- 200 - exp_data$vol_needed_for_PCR
    
    
    return(exp_data)
    
    
}
