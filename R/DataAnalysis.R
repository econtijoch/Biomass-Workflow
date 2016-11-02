#' Main function call to generate biomass data from DNA extraction protocol
#' @param plate_reader_file raw file from plate-reader (can be in .xls format, or .csv), but will only read in first sheet of excel file
#' @param mapping_csv_file a .csv file that contains information as to the location of the samples, the sample types, and their mass. A sample file has been included in this package and can be found at https://github.com/econtijoch/Biomass-Workflow/blob/master/SampleMapping.csv
#' @param exp_id a string that indicates the name of the experiment (used to label the plot of your standard curve)
#' @param standards_plate_reader_file OPTIONAL: if your standards are not on the same plate as your samples, you will need to provide the file path of the plate reader file for the standards (Default is to use same plate as samples)
#' @param standards_mapping_csv_file OPTIONAL: if your standards are not on the same plate as your samples, you will also need to provide the file path of a mapping file for the standards plate (Default is to use same plate as samples)
#' @param num_reads OPTIONAL: the number of reads of each sample from the plate reader (Default = 1)
#' @param volume OPTIONAL: The volume (in uL) used to quantify DNA with Qubit (Default = 2)
#' @param scale OPTIONAL: The scale factor used in the DNA isolation protocol (Defualt = 7, which corresponds to taking 100uL of supernatant from the 700uL of the DIB added). For phenol extraction, use scale = 1.
#' @param shiny OPTIONAL: necessary for running with shiny app interface since filenames are not the same.
#' @param type OPTIONAL: necessary for running with shiny app, must specify file type
#' @param ... Optional arguments to pass
#' @return output table with DNA concentrations, biomass, metadata, and useful computations for downstream (sequencing) applications
#' @export
#'

DataAnalysis <- function(plate_reader_file, mapping_csv_file, exp_id, standards_plate_reader_file = plate_reader_file, 
    standards_mapping_csv_file = mapping_csv_file, num_reads = 1, volume = 2, scale = 7, shiny = FALSE, type = NULL, ...) {
    
    standard_analysis <- StandardAnalysis(standards_plate_reader_file = standards_plate_reader_file, standards_mapping_csv_file = standards_mapping_csv_file, 
        num_reads = num_reads, exp_id = exp_id, shiny, type)
    
    # Read in raw data file from the .csv output of the plate reader. This will produce a data frame with well and read
    # information for the plate.
    
    
    rawdata <- PlateParser(plate_reader_file, num_reads, shiny, type)
    
    # Parse Metadata from mapping file
    mapping <- ParseMappingFile(mapping_csv_file)
    
    
    # Merge data with mapping file, label data appropriately
    data <- merge(rawdata$table, mapping, by = "ReaderWell")
    
    data <- subset(data, !is.na(data$BarcodeID))
    data <- subset(data, data$BarcodeID != "")
    
    rownames(data) <- data$BarcodeID
    
    # Add average fluorescence column to data
    read_start = 2
    read_end = rawdata$num_reads + 1
    
    data$fluor_av <- apply(data[read_start:read_end], 1, mean)
    
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
    exp_data$dna_concentration <- (exp_data[, "fluor_av"] * scale_x + intercept)/volume
    
    # Biomass Analysis
    exp_data$total_dna <- exp_data[, "dna_concentration"] * 0.1
    exp_data$scale_factor <- scale
    exp_data$biomass_ratio <- exp_data$total_dna * scale/exp_data$SampleMass
    exp_data$X16S_possible <- exp_data[, "dna_concentration"] > 1.5
    exp_data$vol_needed_for_PCR <- 400/exp_data[, "dna_concentration"]
    exp_data$water_volume_up_PCR <- 200 - exp_data$vol_needed_for_PCR
    exp_data$metagenomics_possible <- 625/exp_data[, "dna_concentration"] < 28
    exp_data$vol_needed_for_metagenomics <- 625/exp_data[, "dna_concentration"]
    exp_data$water_volume_up_metagenomics <- 25 - exp_data$vol_needed_for_metagenomics
    
    
    return(exp_data)
    
    
}
