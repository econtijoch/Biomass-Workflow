NewDataAnalysis <- function(plate_reader_csv_file, mapping_csv_file, num_reads, exp_id, ...) {
	
	# Parse inputs and override default values if given
	args <- list(...)
	standards_plate_reader_csv_file = plate_reader_csv_file
	standards_mapping_csv_file = mapping_csv_file
	
	if (!is.null(args$standards_plate)) {
		standards_plate_reader_csv_file = args$standards_plate
	}
	
	if(!is.null(args$standards_mapping)) {
		standards_mapping_csv_file = args$standards_mapping
	}


standard_analysis <- NewStandardAnalysis(standards_plate_reader_csv_file = standards_plate_reader_csv_file, standards_mapping_csv_file = standards_mapping_csv_file, num_reads = num_reads exp_id = exp_id) 

# Read in raw data file from the .csv output of the plate reader. This will produce a data frame with well and read information for the plate.
rawdata <- NewPlateParser(plate_reader_csv_file, num_reads)

# Parse Metadata from mapping file
mapping <- ParseMappingFile(mapping_csv_file)


# Merge data with mapping file, label data appropriately
data <- merge(rawdata$table, mapping, by = "Well")

data <- subset(data, !is.na(data$BarcodeID))

rownames(data) <- data$BarcodeID

# Add average fluorescence column to data
read_start = 2
read_end = rawdata$num_reads +1

data$fluor_av <- apply(data[read_start:read_end], 1, mean)

exp_data <- split(data, data$Type)$Experiment

scale_x <- standard_analysis$scale_x
intercept <- standard_analysis$intercept

# Begin working with the data
exp_data$dna_concentration <- (exp_data[, "fluor_av"]*scale_x + intercept)/2

# Biomass Analysis
exp_data$total_dna <- exp_data[, "dna_concentration"]*0.1
exp_data$biomass_ratio <- exp_data$total_dna/exp_data$SampleMass
exp_data$vol_needed_for_PCR <- 400/exp_data[, "dna_concentration"]
exp_data$water_volume_up_PCR <- 200 - exp_data$vol_needed_for_PCR


return(exp_data)


}
