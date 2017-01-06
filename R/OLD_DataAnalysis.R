#' Deprecated function for Data Analysis. See DataAnalysis instead
#' @param plate_reader_csv_file plate reader file (old version)
#' @param mapping_csv_file mapping file
#' @param standards_plate_reader_csv_file plate reader file for plate containing standards (from old machine)
#' @param standards_mapping_csv_file mapping file for plate contianing standards (for wells with standards, Type must be 'Standard')
#' @param exp_id experiment id
#' @param volume qubit volume
#' @param scale subsampling scaling factor
#' @param print print standards information
#' @param ... other arguments
#' @return list with a data table with biomass data calculated and the standards information
#' @export
#'

Deprecated_DataAnalysis <- function(plate_reader_csv_file, mapping_csv_file, exp_id, standards_plate_reader_csv_file = plate_reader_csv_file, 
    standards_mapping_csv_file = mapping_csv_file, volume = 2, scale = 1, print = FALSE, ...) {
    
    
    standard_analysis <- Deprecated_StandardAnalysis(standards_plate_reader_csv_file = standards_plate_reader_csv_file, 
        standards_mapping_csv_file = standards_mapping_csv_file, exp_id = exp_id, print = print)
    
    # Read in raw data file from the .csv output of the plate reader. This will produce a data frame with well and read
    # information for the plate.
    rawdata <- Deprecated_ParsePlateReaderFile(plate_reader_csv_file)
    
    # Parse Metadata from mapping file
    mapping <- ParseMappingFile(mapping_csv_file)
    
    
    # Merge data with mapping file, label data appropriately
    data <- merge(rawdata, mapping, by = "ReaderWell")
    
    data <- subset(data, !is.na(data$BarcodeID))
    data <- subset(data, data$BarcodeID != "")
    
    rownames(data) <- data$BarcodeID
    
    exp_data <- split(data, data$Type)$Experiment
    
    exp_data <- exp_data %>% dplyr::mutate(Other = ifelse(SampleMass < 10, "No_Pellet", NA))
    
    "%ni%" <- Negate("%in%")
    
    if ("Experiment" %ni% colnames(exp_data)) {
      exp_data$Experiment <- exp_id
    }
    
    scale_x <- standard_analysis$scale_x
    intercept <- standard_analysis$intercept
    
    # Begin working with the data
    exp_data$qubit_volume <- volume
    exp_data$dna_concentration <- (exp_data[, "Fluorescence"] * scale_x + intercept)/volume
    
    # Biomass Analysis
    exp_data$total_dna <- exp_data[, "dna_concentration"] * 0.1
    exp_data$scale_factor <- scale
    exp_data$biomass_ratio <- exp_data$total_dna * scale/exp_data$SampleMass
    exp_data$X16S_possible <- (exp_data[, "dna_concentration"] > 1.5)  & (exp_data[, 'dna_concentration'] > 0)
    exp_data$vol_needed_for_PCR <- 400/exp_data[, "dna_concentration"]
    exp_data$water_volume_up_PCR <- 200 - exp_data$vol_needed_for_PCR
    exp_data$metagenomics_possible <- (625/exp_data[, "dna_concentration"] < 28) & (exp_data[, 'dna_concentration'] > 0)
    exp_data$vol_needed_for_metagenomics <- 625/exp_data[, "dna_concentration"]
    exp_data$water_volume_up_metagenomics <- 25 - exp_data$vol_needed_for_metagenomics
    
    output_list <- list(data = exp_data, standards_plot = standard_analysis$plot)
    
    return(output_list)
    
    
}
