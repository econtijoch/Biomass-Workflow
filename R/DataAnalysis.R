#' Main function call to generate microbial density data from DNA extraction protocol
#' @param plate_reader_file raw file from plate-reader (can be in .xls format, or .csv), but will only read in first sheet of excel file
#' @param mapping_csv_file a .csv file that contains information as to the location of the samples, the sample types, and their mass. A sample file has been included in this package and can be found at https://github.com/econtijoch/Biomass-Workflow/blob/master/SampleMapping.csv
#' @param exp_id a string that indicates the name of the experiment (used to label the plot of your standard curve)
#' @param standards_plate_reader_file OPTIONAL: if your standards are not on the same plate as your samples, you will need to provide the file path of the plate reader file for the standards (Default is to use same plate as samples)
#' @param standards_mapping_csv_file OPTIONAL: if your standards are not on the same plate as your samples, you will also need to provide the file path of a mapping file for the standards plate (Default is to use same plate as samples)
#' @param volume OPTIONAL: The volume (in uL) used to quantify DNA with Qubit (Default = 2)
#' @param scale OPTIONAL: The scale factor used in the DNA isolation protocol (Defualt = 3.5, which corresponds to taking 200uL of supernatant from the 700uL of the DIB added). For phenol extraction, use scale = 1.
#' @param shiny OPTIONAL: necessary for running with shiny app interface since filenames are not the same.
#' @param type_plate OPTIONAL: necessary for running with shiny app, must specify file type of plate
#' @param type_standards_plate OPTIONAL: necessary for running with shiny app, must specify type of plate for standards
#' @param print OPTIONAL: whether or not to save a copy of the standards curve to the working directory
#' @param ... Optional arguments to pass
#' @return output table with DNA concentrations, microbial density, metadata, and useful computations for downstream (sequencing) applications, and a plot of the standard curve
#' @export
#'

DataAnalysis <- function(plate_reader_file, mapping_csv_file, exp_id = "EXP_ID", standards_plate_reader_file = plate_reader_file, 
    standards_mapping_csv_file = mapping_csv_file, volume = 2, scale = 3.5, shiny = FALSE, type_plate = NULL, type_standards_plate = NULL, print = FALSE, ...) {
    
    standard_analysis <- StandardAnalysis(standards_plate_reader_file = standards_plate_reader_file, standards_mapping_csv_file = standards_mapping_csv_file, 
         exp_id = exp_id, shiny = shiny, type = type_standards_plate, print = print)
    
    # Read in raw data file from the .csv output of the plate reader. This will produce a data frame with well and read
    # information for the plate.
    
    
    rawdata <- PlateParser(plate_reader_file, shiny, type_plate)
    
    # Parse Metadata from mapping file
    mapping <- ParseMappingFile(mapping_csv_file)
    
    
    # Merge data with mapping file, label data appropriately
    data <- merge(rawdata, mapping, by = "ReaderWell")
    
    data <- subset(data, !is.na(data$BarcodeID))
    data <- subset(data, data$BarcodeID != "")
    
    rownames(data) <- data$BarcodeID
    
    exp_data <- split(data, data$Type)$Experiment
    
    exp_data <- exp_data %>% dplyr::mutate(Other = ifelse(SampleMass < 5, "No_Pellet", NA))
    
    "%ni%" <- Negate("%in%")
    
    if ("Experiment" %ni% colnames(exp_data)) {
        exp_data$Experiment <- exp_id
    }
    
    scale_x <- standard_analysis$scale_x
    intercept <- standard_analysis$intercept
    
    # Begin working with the data
	output_data <- exp_data %>% dplyr::mutate(qubit_volume = volume, 
		dna_concentration = (Fluorescence * scale_x + intercept)/volume, 
		total_dna = dna_concentration * 0.1, 
		scale_factor = scale, 
		microbial_density = (total_dna * scale)/SampleMass, 
		X16S_possible = (dna_concentration > 1.5 & dna_concentration > 0),
		vol_needed_for_PCR = 400/dna_concentration,
		water_volume_up_PCR = 200 - vol_needed_for_PCR,
		metagenomics_possible =  dna_concentration >= 20,
		vol_needed_for_metagenomics = 500/dna_concentration,
		water_volume_up_metagenomics = 25 - vol_needed_for_metagenomics)
		
    
    output_list <- list(data = output_data, standards_plot = standard_analysis$plot)
    
    return(output_list)
    
    
}
