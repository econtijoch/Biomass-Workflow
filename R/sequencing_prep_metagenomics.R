#' Function to produce files necessary to generate metagenomic sequencing runs that utilizes the Beckmann robot for dilutions and creates sequencing mapping files in QIIME-ready format, from multiple experiment data sets.
#' @param experiment_data_list an input list that contains the data from the samples you want to sequence (ideally, the output from DataAnalysis, or combinations of them)
#' @param n_barcode_plates the number of barcode plates available (i.e. if you have 96 unique sequencing barcodes, this would be 1)
#' @param barcode_map_file a string of the path to a file containing the barcode sequences, linker primer sequences, and barcode plate and well locations (see example data)
#' @param output_directory OPTIONAL: a path to the directory for your desired output (Default = working directory)
#' @param filter OPTIONAL: a boolean that indicates whether or not to filter out samples that have not passed the QC check from the DataAnalysis function (Default = TRUE)
#' @return A list containing the data for as many sequencing runs necessary to sequence all of your samples. For each sequencing run, there will be a list of plates, corresponding to the data frames that contain the information necessary to use the Beckmann robot to dilute samples. There will also be a mapping file, and a list of samples included in that sequencing run.
#' @export
#'

sequencing_prep_metagenomics <- function(experiment_data_list, n_barcode_plates, barcode_map_file = NULL, output_directory = getwd(), 
    filter = T) {

		current_directory <- getwd()
		if (current_directory == output_directory) {
			output_path <- output_directory
		}
		else {
			output_path <- output_directory
		}
		if (!file.exists(output_path)) {
		 dir.create(file.path(output_path))
	 	}
	
    # Useful for adding barcode metadata
    barcode_info <- data.frame()
    for (i in 1:n_barcode_plates) {
        for (j in 1:96) {
            entry <- ((i - 1) * 96) + j
            barcode_info[entry, "BarcodePlate"] <- paste("Plate", i, sep = "")
            row <- (j - 1)%/%12
            column <- j - (12 * (row))
            row_id <- c("A", "B", "C", "D", "E", "F", "G", "H")
            barcode_info[entry, "BarcodeWell"] <- paste(row_id[row + 1], sprintf("%02d", column), sep = "")
        }
    }
    
    if (!is.null(barcode_map_file)) {
      barcodes <- utils::read.csv(barcode_map_file)
    } else {
      barcodes <- data.frame(BarcodePlate = barcode_info$BarcodePlate, BarcodeWell = barcode_info$BarcodeWell, BarcodeSequence = "", LinkerPrimerSequence = "")
    }
    
    if (filter == T) {
        sequencing_combined <- Reduce(function(...) suppressWarnings(dplyr::full_join(...)), experiment_data_list) %>% 
            dplyr::filter(dna_concentration >= 20)
        cat("WARNING: By default, any samples with DNA concentration < 20 ng/uL will be filtered and not sequenced. To overcome this, pass \"filter = FALSE\" as an argument to the function call.\n")
    } else {
        sequencing_combined <- Reduce(function(...) suppressWarnings(dplyr::full_join(...)), experiment_data_list)
    }
    
    robot_dilution <- robot_prep_metagenomics(sequencing_combined, n_barcode_plates)
    
    sample_map <- robot_dilution %>% dplyr::select(BarcodeID, SequencingRun, BarcodePlate, BarcodeWell)
    
    n_sequencing_runs <- ceiling(nrow(sequencing_combined)/(n_barcode_plates * 96))
    
    cat(paste("\n---------------\nNumber of Samples :", nrow(robot_dilution), "\n---------------"))
    cat(paste("\nNumber of Sequencing Runs Needed: ", n_sequencing_runs, "\n---------------", sep = ""))
    cat(paste("\nNumber of Unused Barcodes Available: ", (96 * n_barcode_plates * n_sequencing_runs) - nrow(robot_dilution), 
        "\n---------------", sep = ""))
    
    sequencing_runs <- list()
    
    for (i in 1:n_sequencing_runs) {
        sequencing_id <- paste("Metagenomics_Run", i, sep = "")
        sequencing_runs[[sequencing_id]] <- list()
        
        if (!file.exists(file.path(output_path, sequencing_id))) {
            dir.create(file.path(output_path, sequencing_id))
        }
        sequencing_runs[[sequencing_id]][["Plates"]] <- list()
        
        if (i < n_sequencing_runs) {
            plates_in_run <- n_barcode_plates
        } else {
            plates_in_run <- ceiling(nrow(robot_dilution)/96) - n_barcode_plates * (i - 1)
        }
        
        for (j in 1:plates_in_run) {
            plate_id <- paste("Plate", j, sep = "")
            sequencing_runs[[sequencing_id]][["Plates"]][[plate_id]] <- robot_dilution %>% dplyr::filter(SequencingRun == 
                sequencing_id, BarcodePlate == paste("Plate", j, sep = ""))
            utils::write.csv(x = sequencing_runs[[sequencing_id]][["Plates"]][[plate_id]], file = paste(file.path(output_path, sequencing_id), "/", sequencing_id, 
                "_", plate_id, "_robot_file.csv", sep = ""), row.names = F)
        }
        sequencing_runs[[sequencing_id]][["Samples"]] <- unlist(lapply(sequencing_runs[[sequencing_id]][["Plates"]], 
            function(x) x[["BarcodeID"]]), use.names = FALSE)
        samples <- sequencing_runs[[sequencing_id]][["Samples"]]
        sequencing_runs[[sequencing_id]][["Mapping"]] <- suppressWarnings(dplyr::filter(sequencing_combined, BarcodeID %in% samples) %>% 
            dropcolumns(.) %>% dplyr::left_join(., sample_map, by = "BarcodeID") %>% dplyr::left_join(., barcodes, by = c("BarcodePlate", 
            "BarcodeWell")) %>% dplyr::mutate(SampleID = gsub("[[:punct:]]", ".", BarcodeID)) %>% dplyr::select(SampleID, 
            BarcodeSequence, LinkerPrimerSequence, SequencingRun, BarcodePlate, BarcodeWell, dplyr::everything(), -Fluorescence, 
            -BarcodeID, -ReaderWell, -Type) %>% dplyr::mutate(Description = SampleID))
        
        
        utils::write.table(x = "#", file = paste(file.path(output_path, sequencing_id), "/", sequencing_id, "_sequencing_mapping.txt", sep = ""), row.names = F, 
            sep = "\t", quote = F, col.names = F, eol = "")
        suppressWarnings(utils::write.table(x = sequencing_runs[[sequencing_id]][["Mapping"]], file = paste(file.path(output_path, sequencing_id), "/", sequencing_id, 
            "_sequencing_mapping.txt", sep = ""), row.names = F, sep = "\t", quote = F, append = T, na = "NA"))
    }
    
    return(sequencing_runs)
    
}
