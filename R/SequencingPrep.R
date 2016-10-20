# Useful for slimming data tables
dropcolumns <- function(table) {
  return(table[colSums(!is.na(table)) > 0])
}

sequencing_prep_16S <- function(experiment_data_list, n_barcode_plates, barcode_map_file, output_directory, filter = T) {
	setwd(output_directory)
	# Useful for adding barcode metadata
	barcode_info <- data.frame()
	for (i in 1:n_barcode_plates) {
	  for (j in 1:96) {
	    entry <- ((i-1)*96)+j 
	    barcode_info[entry, "BarcodePlate"] <- paste("Plate", i, sep = "")
	    row <- (j-1) %/% 12
	    column <- j - (12*(row))
	    row_id <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
	    barcode_info[entry, "BarcodeWell"] <- paste(row_id[row+1], sprintf("%02d", column), sep = "")
	  }
	}
	
	barcodes <- read.csv(barcode_map_file)
	
	if (filter == T) {
		sequencing_combined  <- Reduce(function(...) suppressMessages(dplyr::full_join(...)), experiment_data_list) %>% dplyr::filter(X16S_possible == TRUE)
		cat('WARNING: By default, any samples with DNA concentration < 1.5 ng/uL will be filtered and not sequenced. To overcome this, pass "filter = FALSE" as an argument to the function call.\n')
	} else {
		sequencing_combined  <- Reduce(function(...) suppressMessages(dplyr::full_join(...)), experiment_data_list)
	}

	robot_dilution <- robot_prep_16S(sequencing_combined, n_barcode_plates)

	sample_map <- robot_dilution %>% dplyr::select(BarcodeID, SequencingRun, BarcodePlate, BarcodeWell)

	n_sequencing_runs <- ceiling(nrow(sequencing_combined)/(n_barcode_plates*96))

	cat(paste("\n---------------\nNumber of Samples :", nrow(robot_dilution), "\n---------------"))
	cat(paste("\nNumber of Sequencing Runs Needed: ", n_sequencing_runs, "\n---------------", sep = ""))
	cat(paste("\nNumber of Unused Barcodes Available: ", (96*n_barcode_plates*n_sequencing_runs) - nrow(robot_dilution), "\n---------------", sep = ""))

	sequencing_runs <- list()

	for (i in 1:n_sequencing_runs) {
	  sequencing_id <- paste("16S_Run", i, sep = "")
	  sequencing_runs[[sequencing_id]] <- list()
  
	  if (file.exists(sequencing_id)){
	    setwd(file.path(output_directory, sequencing_id))
	  } else {
	    dir.create(file.path(output_directory, sequencing_id))
	    setwd(file.path(output_directory, sequencing_id))
	  }
	  sequencing_runs[[sequencing_id]][["Plates"]] <- list()
  
	  if (ceiling(nrow(sequencing_combined)/96) > n_barcode_plates & i < 2) {
	    plates_in_run <- n_barcode_plates
	  } else {
	    plates_in_run <- ceiling(nrow(sequencing_combined)/96) - (i-1)*n_barcode_plates
	  }
  
	  for (j in 1:plates_in_run) {
	    plate_id <- paste("Plate", j, sep = "")
	    sequencing_runs[[sequencing_id]][["Plates"]][[plate_id]] <- robot_dilution %>% dplyr::filter(SequencingRun == sequencing_id, BarcodePlate == paste("Plate", j, sep = ""))
	    write.csv(x = sequencing_runs[[sequencing_id]][["Plates"]][[plate_id]], file = paste(sequencing_id, "_", plate_id, "_robot_file.csv", sep = ""), row.names = F)
	  }
	  sequencing_runs[[sequencing_id]][["Samples"]] <- unlist(lapply(sequencing_runs[[sequencing_id]][['Plates']], function (x) x[['BarcodeID']]), use.names = FALSE)
	  samples <- sequencing_runs[[sequencing_id]][["Samples"]]
	  sequencing_runs[[sequencing_id]][["Mapping"]] <- dplyr::filter(sequencing_combined, BarcodeID %in% samples) %>% dropcolumns(.) %>% dplyr::left_join(., sample_map, by = 'BarcodeID') %>% dplyr::left_join(., barcodes, by = c('BarcodePlate', 'BarcodeWell')) %>% dplyr::mutate(SampleID = gsub("[[:punct:]]", ".", BarcodeID)) %>% dplyr::select(SampleID, BarcodeSequence, LinkerPrimerSequence, SequencingRun, BarcodePlate, BarcodeWell, everything(), -Read.1, -BarcodeID, -ReaderWell, -Type) %>% dplyr::mutate(Description = SampleID)
  
  
	  write.table(x = "#", file = paste(sequencing_id, "_sequencing_mapping.txt", sep = ""), row.names = F, sep = "\t", quote = F, col.names = F, eol = "")
	  suppressWarnings(write.table(x = sequencing_runs[[sequencing_id]][["Mapping"]], file = paste(sequencing_id, "_sequencing_mapping.txt", sep = ""), row.names = F, sep = "\t", quote = F, append = T, na = 'NA'))
	  setwd('../')
	}

	return(sequencing_runs)

}

sequencing_prep_metagenomics <- function(experiment_data_list, n_barcode_plates, barcode_map_file, output_directory, filter = T) {
	setwd(output_directory)
	# Useful for adding barcode metadata
	barcode_info <- data.frame()
	for (i in 1:n_barcode_plates) {
	  for (j in 1:96) {
	    entry <- ((i-1)*96)+j 
	    barcode_info[entry, "BarcodePlate"] <- paste("Plate", i, sep = "")
	    row <- (j-1) %/% 12
	    column <- j - (12*(row))
	    row_id <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
	    barcode_info[entry, "BarcodeWell"] <- paste(row_id[row+1], sprintf("%02d", column), sep = "")
	  }
	}
	
	barcodes <- read.csv(barcode_map_file)
	
	if (filter == T) {
		sequencing_combined  <- Reduce(function(...) suppressMessages(dplyr::full_join(...)), experiment_data_list) %>% dplyr::filter(metagenomics_possible == TRUE)
		cat('WARNING: By default, any samples with DNA concentration < 22 ng/uL will be filtered and not sequenced. To overcome this, pass "filter = FALSE" as an argument to the function call.\n')
	} else {
		sequencing_combined  <- Reduce(function(...) suppressMessages(dplyr::full_join(...)), experiment_data_list)
	}

	robot_dilution <- robot_prep_metagenomics(sequencing_combined, n_barcode_plates)

	sample_map <- robot_dilution %>% dplyr::select(BarcodeID, SequencingRun, BarcodePlate, BarcodeWell)

	n_sequencing_runs <- ceiling(nrow(sequencing_combined)/(n_barcode_plates*96))

	cat(paste("\n---------------\nNumber of Samples :", nrow(robot_dilution), "\n---------------"))
	cat(paste("\nNumber of Sequencing Runs Needed: ", n_sequencing_runs, "\n---------------", sep = ""))
	cat(paste("\nNumber of Unused Barcodes Available: ", (96*n_barcode_plates*n_sequencing_runs) - nrow(robot_dilution), "\n---------------", sep = ""))

	sequencing_runs <- list()

	for (i in 1:n_sequencing_runs) {
	  sequencing_id <- paste("Metagenomics_Run", i, sep = "")
	  sequencing_runs[[sequencing_id]] <- list()
  
	  if (file.exists(sequencing_id)){
	    setwd(file.path(output_directory, sequencing_id))
	  } else {
	    dir.create(file.path(output_directory, sequencing_id))
	    setwd(file.path(output_directory, sequencing_id))
	  }
	  sequencing_runs[[sequencing_id]][["Plates"]] <- list()
  
	  if (ceiling(nrow(sequencing_combined)/96) > n_barcode_plates & i < (n_sequencing_runs-1)) {
	    plates_in_run <- n_barcode_plates
	  } else {
	    plates_in_run <- ceiling(nrow(sequencing_combined)/96) - (i-1)*n_barcode_plates
	  }
  
	  for (j in 1:plates_in_run) {
	    plate_id <- paste("Plate", j, sep = "")
	    sequencing_runs[[sequencing_id]][["Plates"]][[plate_id]] <- robot_dilution %>% dplyr::filter(SequencingRun == sequencing_id, BarcodePlate == paste("Plate", j, sep = ""))
	    write.csv(x = sequencing_runs[[sequencing_id]][["Plates"]][[plate_id]], file = paste(sequencing_id, "_", plate_id, "_robot_file.csv", sep = ""), row.names = F)
	  }
	  sequencing_runs[[sequencing_id]][["Samples"]] <- unlist(lapply(sequencing_runs[[sequencing_id]][['Plates']], function (x) x[['BarcodeID']]), use.names = FALSE)
	  samples <- sequencing_runs[[sequencing_id]][["Samples"]]
	  sequencing_runs[[sequencing_id]][["Mapping"]] <- dplyr::filter(sequencing_combined, BarcodeID %in% samples) %>% dropcolumns(.) %>% dplyr::left_join(., sample_map, by = 'BarcodeID') %>% dplyr::left_join(., barcodes, by = c('BarcodePlate', 'BarcodeWell')) %>% dplyr::mutate(SampleID = gsub("[[:punct:]]", ".", BarcodeID)) %>% dplyr::select(SampleID, BarcodeSequence, LinkerPrimerSequence, SequencingRun, BarcodePlate, BarcodeWell, everything(), -Read.1, -BarcodeID, -ReaderWell, -Type) %>% dplyr::mutate(Description = SampleID)
  
  
	  write.table(x = "#", file = paste(sequencing_id, "_sequencing_mapping.txt", sep = ""), row.names = F, sep = "\t", quote = F, col.names = F, eol = "")
	  suppressWarnings(write.table(x = sequencing_runs[[sequencing_id]][["Mapping"]], file = paste(sequencing_id, "_sequencing_mapping.txt", sep = ""), row.names = F, sep = "\t", quote = F, append = T, na = 'NA'))
	  setwd('../')
	}

	return(sequencing_runs)

}