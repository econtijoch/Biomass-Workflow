## Prepare for robot
require(plyr)
require(dplyr)


well_parser <- function(well) {
  row <- substring(well, 1,1)
  column <- as.numeric(substring(well, 2,3))
  num_row <- switch(EXPR = row, A = 1, B = 2, C = 3, D = 4, E = 5, F = 6, G = 7, H = 8)
  output <- 12*(num_row-1) + column
  return(output)
}


robot_prep_16S <- function(dataset, n_barcode_plates) {
  
	if ("BarcodePlate" %ni% colnames(dataset)) {
		sample_number <- nrow(dataset)
		total_number_of_plates_needed <- ceiling(sample_number/96)
		number_of_runs_needed <- ceiling(sample_number/(96*n_barcode_plates))
		
		dataset$BarcodePlate <- ""
		dataset$BarcodeWell <- ""
		dataset$SequencingRun <- ""
		
		for (k in 1:number_of_runs_needed) {
			if (total_number_of_plates_needed > n_barcode_plates & k < 2) {
			    plates_in_run <- n_barcode_plates
			  } else {
			    plates_in_run <-  total_number_of_plates_needed - (k-1)*n_barcode_plates
			  }			
			for (i in 1:plates_in_run) {
			  for (j in 1:96) {
			    entry <- ((i-1)*96)+j + (k-1)*n_barcode_plates*96
			    if (entry <= sample_number) {
			    	dataset[entry, "BarcodePlate"] <- paste("Plate", i, sep = "")
			    	row <- (j-1) %/% 12
			   	 	column <- j - (12*(row))
			    	row_id <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
			    	dataset[entry, "BarcodeWell"] <- paste(row_id[row+1], sprintf("%02d", column), sep = "")
					dataset[entry, "SequencingRun"] <- paste('16S_Run', k, sep = "")
			    }
				
			  }
  
			}
			
		}
	}
  
	if ("X16S_possible" %in% colnames(dataset)) {
		if (sum(dataset[,"X16S_possible"]) < nrow(dataset)) {
			cat("WARNING: Running robot prep on samples that have not passed the '16S possible' check. Re-check data and filter if necessary.")
		}
	}
  
  robot_table <- dataset %>% select(BarcodeID, PlateID, SampleWell, vol_needed_for_PCR, water_volume_up_PCR, dna_concentration, BarcodePlate, BarcodeWell, SequencingRun)
  robot_table$BarcodeID <- as.character(robot_table$BarcodeID)
  robot_table$PlateID <- as.character(robot_table$PlateID)
  robot_table$Warning <- ""
  robot_table$DNASource <- paste("Unnormalized", robot_table$PlateID, sep = "_")
  robot_table$StartingConc <- robot_table$dna_concentration
  robot_table$FinalConc <- 0
  
  for (i in 1:nrow(robot_table)) {
	  if (robot_table[i, 'dna_concentration'] < 1.5) {
	      robot_table[i, "vol_needed_for_PCR"] = 40
	      robot_table[i, 'water_volume_up_PCR'] = 0
	      robot_table[i, 'Warning'] = '[WARNING] **DID NOT PASS 16S_possible check. Starting Concentration < 1.5: Transferred 40 uL + 0 uL Water/EB'
	      robot_table[i, "FinalConc"] = robot_table[i, "StartingConc"]
	  } else if (robot_table[i, 'dna_concentration'] < 2) {
	      robot_table[i, "vol_needed_for_PCR"] = 40
	      robot_table[i, 'water_volume_up_PCR'] = 0
	      robot_table[i, 'Warning'] = '[WARNING] 2 > Starting Concentration > 1.5: Transferred 40 uL + 0 uL Water/EB'
	      robot_table[i, "FinalConc"] = robot_table[i, "StartingConc"]
	  } else if (robot_table[i, 'dna_concentration'] < 20) {
	      robot_table[i, "vol_needed_for_PCR"] = robot_table[i, "vol_needed_for_PCR"]/5
	      robot_table[i, 'water_volume_up_PCR'] = robot_table[i, "water_volume_up_PCR"]/5
	      robot_table[i, 'Warning'] = '20 > Starting Concentration > 2: Transferred [DNAvol + Water/EB]= 40 uL'
	      robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*robot_table[i, "vol_needed_for_PCR"])/40
	  } else if (robot_table[i, 'dna_concentration'] < 100) {
	      robot_table[i, "vol_needed_for_PCR"] = robot_table[i, "vol_needed_for_PCR"]/2
	      robot_table[i, 'water_volume_up_PCR'] = robot_table[i, "water_volume_up_PCR"]/2
	      robot_table[i, 'Warning'] = '100 > Starting Concentration > 20: Transferred [DNAvol + Water/EB]= 100 uL'
	      robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*robot_table[i, "vol_needed_for_PCR"])/100
	  } else if (robot_table[i, 'dna_concentration'] < 400) {
	      robot_table[i, "vol_needed_for_PCR"] = robot_table[i, "vol_needed_for_PCR"]*3/4
	      robot_table[i, 'water_volume_up_PCR'] = robot_table[i, "water_volume_up_PCR"]*3/4
	      robot_table[i, 'Warning'] = '400 > Starting Concentration > 100: Transferred [DNAvol + Water/EB]= 150 uL'
	      robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*robot_table[i, "vol_needed_for_PCR"])/150
	  } else {
	      robot_table[i, "vol_needed_for_PCR"] = 1
	      robot_table[i, 'water_volume_up_PCR'] = 99
	      robot_table[i, 'Warning'] = '[WARNING] Starting Concentration > 400: Transferred 1uL DNA + 199 uL Water/EB'
	      robot_table[i, "FinalConc"] = robot_table[i, "StartingConc"]/200
	  }
	 
    robot_table$DNASourceWell <- unlist(lapply(robot_table$SampleWell, well_parser))
    
    robot_table$DNA_Vol <- round_any(robot_table$vol_needed_for_PCR, 0.1)
    robot_table$Water_Vol <- round_any(robot_table$water_volume_up_PCR, 0.1)
    robot_table$Destination <- paste("Normalized", robot_table$BarcodePlate, sep = "_")
    robot_table$DestinationWell <- unlist(lapply(robot_table$BarcodeWell, well_parser))
    robot_table$WaterSource <- 'WaterSource'
    robot_table$WaterWell <- 1
    robot_table$NormalizedVolume <- robot_table$DNA_Vol + robot_table$Water_Vol
    
    output <- as.data.frame(select(robot_table, WaterSource, WaterWell, DNASource, DNASourceWell, Destination, DestinationWell, DNA_Vol, Water_Vol, Warning, BarcodeID, PlateID, SampleWell, SequencingRun, BarcodePlate, BarcodeWell, NormalizedVolume, StartingConc, FinalConc))
    
  }
  output <- output  %>% dplyr::arrange(SequencingRun, BarcodePlate, DestinationWell)
  
  cat("Names of plates needed: \n", paste(levels(as.factor(output$DNASource)), collapse = "\n"))
  
  return(output)
  
  
}


"%ni%" <- Negate("%in%")

robot_prep_metagenomics <- function(dataset, n_barcode_plates) {
	
	"%ni%" <- Negate("%in%")
	
	
	if ("BarcodePlate" %ni% colnames(dataset)) {
		sample_number <- nrow(dataset)
		number_of_plates_needed <- ceiling(sample_number/96)
		dataset$BarcodePlate <- ""
		dataset$BarcodeWell <- ""
		for (i in 1:number_of_plates_needed) {
		  for (j in 1:96) {
		    entry <- ((i-1)*96)+j 
		    if (entry <= sample_number) {
		    	dataset[entry, "BarcodePlate"] <- paste("Plate", i, sep = "")
		    	row <- (j-1) %/% 12
		   	 	column <- j - (12*(row))
		    	row_id <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
		    	dataset[entry, "BarcodeWell"] <- paste(row_id[row+1], sprintf("%02d", column), sep = "")
		    }
		  }
  
		}
	}
	
	if ("metagenomics_possible" %in% colnames(dataset)) {
		if (sum(dataset$metagenomics_possible) < nrow(dataset)) {
			cat("WARNING: Running robot prep on samples that have not passed the 'metagenomics possible' check. Re-check data and filter if necessary.")
		}
	}
  
  robot_table <- dataset %>% select(BarcodeID, PlateID, SampleWell, vol_needed_for_metagenomics, water_volume_up_metagenomics, dna_concentration, BarcodePlate, BarcodeWell)
  robot_table$BarcodeID <- as.character(robot_table$BarcodeID)
  robot_table$PlateID <- as.character(robot_table$PlateID)
  robot_table$Warning <- ""
  robot_table$DNASource <- paste("Unnormalized", robot_table$PlateID, sep = "_")
  robot_table$StartingConc <- robot_table$dna_concentration
  robot_table$FinalConc <- 0
  robot_table$TotalDNAin20uL <- 0
  
  for (i in 1:nrow(robot_table)) {
   if (robot_table[i, "vol_needed_for_metagenomics"] < 1) {
      robot_table[i, "vol_needed_for_metagenomics"] <- 1
	  robot_table[i, "water_volume_up_metagenomics"] <- 24
      robot_table[i, "Warning"] <- "[WARNING] DNAvol < 1: Transferred 1 uL + 24 uL Water/EB"
	  robot_table[i, "FinalConc"] <- (robot_table[i, "StartingConc"]*1)/(25)
	  robot_table[i, "TotalDNAin20uL"] <- robot_table[i, 'FinalConc']*20
    }
	else if (robot_table[i, 'vol_needed_for_metagenomics'] > 25) {
		robot_table[i, "vol_needed_for_metagenomics"] <- 25
		robot_table[i, 'water_volume_up_metagenomics'] <- 0
		robot_table[i, 'Warning'] <- '[WARNING] DNAvol > 25: Transferred 25 uL + 0 uL Water/EB'
		robot_table[i, "FinalConc"] <- (robot_table[i, "StartingConc"]*25)/(25)
		robot_table[i, "TotalDNAin20uL"] <- robot_table[i, 'FinalConc']*20
	}
	else {
		robot_table[i, 'Warning'] <- '25 > DNAvol > 1: Transferred [DNAvol + Water/EB]= 25 uL (Default)'
		robot_table[i, "FinalConc"] <- (robot_table[i, "StartingConc"]*robot_table[i, "vol_needed_for_metagenomics"])/(25)
		robot_table[i, "TotalDNAin20uL"] <- robot_table[i, 'FinalConc']*20
	}
	 
    robot_table$DNASourceWell <- unlist(lapply(robot_table$SampleWell, well_parser))
    
    robot_table$DNA_Vol <- round_any(robot_table$vol_needed_for_metagenomics, 0.1)
    robot_table$Water_Vol <- round_any(robot_table$water_volume_up_metagenomics, 0.1)
    robot_table$Destination <- paste("Normalized", robot_table$BarcodePlate, sep = "_")
    robot_table$DestinationWell <- unlist(lapply(robot_table$BarcodeWell, well_parser))
    robot_table$WaterSource <- 'WaterSource'
    robot_table$WaterWell <- 1
    robot_table$NormalizedVolume <- robot_table$DNA_Vol + robot_table$Water_Vol
    
    output <- as.data.frame(select(robot_table, WaterSource, WaterWell, DNASource, DNASourceWell, Destination, DestinationWell, DNA_Vol, Water_Vol, Warning, BarcodeID, PlateID, SampleWell, BarcodePlate, BarcodeWell, NormalizedVolume, StartingConc, FinalConc, TotalDNAin20uL))
    
    
  }
  output <- output %>% arrange(Destination)
  
  cat("Names of plates needed: \n", paste(levels(as.factor(output$DNASource)), collapse = "\n"))
  
  return(output)
  
  
}