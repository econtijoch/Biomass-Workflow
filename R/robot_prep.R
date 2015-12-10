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


robot_prep <- function(dataset, output_name) {
  
  robot_table <- dataset %>% select(BarcodeID, PlateID, SampleWell, vol_needed_for_PCR, water_volume_up_PCR, dna_concentration, BarcodePlate, BarcodeWell)
  robot_table$BarcodeID <- as.character(robot_table$BarcodeID)
  robot_table$PlateID <- as.character(robot_table$PlateID)
  robot_table$Warning <- ""
  robot_table$DNASource <- paste("Unnormalized", robot_table$PlateID, sep = "_")
  robot_table$StartingConc <- robot_table$dna_concentration
  robot_table$FinalConc <- 0
  
  for (i in 1:nrow(robot_table)) {
    if (robot_table[i, 'dna_concentration'] < 0) {
      robot_table[i, "vol_needed_for_PCR"] = 40
      robot_table[i, 'water_volume_up_PCR'] = 0
      robot_table[i, 'Warning'] = '[WARNING] Starting Concentration < 0: Transferred 40 uL + 0 uL Water/EB'
      robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*40)/(40)
    }
    else if (robot_table[i, "vol_needed_for_PCR"] < 1) {
      robot_table[i, "vol_needed_for_PCR"] = 1
	  robot_table[i, "water_volume_up_PCR"] = 199
      robot_table[i, "Warning"] = "[WARNING] DNAvol < 1: Transferred 1 uL + 199 uL Water/EB"
	  robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*1)/(200)
    }
	else if (robot_table[i, 'vol_needed_for_PCR'] > 150) {
		robot_table[i, "vol_needed_for_PCR"] = 40
		robot_table[i, 'water_volume_up_PCR'] = 0
		robot_table[i, 'Warning'] = '[WARNING] DNAvol > 150: Transferred 40 uL + 0 uL Water/EB'
		robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*40)/(40)
	}
	else if (robot_table[i, 'vol_needed_for_PCR'] > 50) {
		robot_table[i, "vol_needed_for_PCR"] = robot_table[i, "vol_needed_for_PCR"]/4
		robot_table[i, 'water_volume_up_PCR'] = robot_table[i, "water_volume_up_PCR"]/4
		robot_table[i, 'Warning'] = '[WARNING] 150 > DNAvol > 50: Transferred [DNAvol + Water/EB]= 50 uL'
		robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*robot_table[i, "vol_needed_for_PCR"])/(50)
	}
    else if (robot_table[i, "vol_needed_for_PCR"] > 20) {
	  robot_table[i, "vol_needed_for_PCR"] = robot_table[i, "vol_needed_for_PCR"]/2
	  robot_table[i, 'water_volume_up_PCR'] = robot_table[i, "water_volume_up_PCR"]/2
	  robot_table[i, 'Warning'] = '[WARNING] 50 > DNAvol > 20: Transferred [DNAvol + Water/EB]= 100 uL'
	  robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*robot_table[i, "vol_needed_for_PCR"])/(100)
    }
	else {
		robot_table[i, 'Warning'] = '20 > DNAvol > 1: Transferred [DNAvol + Water/EB]= 200 uL (Default)'
		robot_table[i, "FinalConc"] = (robot_table[i, "StartingConc"]*robot_table[i, "vol_needed_for_PCR"])/(200)
	}
	 
    robot_table$DNASourceWell <- unlist(lapply(robot_table$SampleWell, well_parser))
    
    robot_table$DNA_Vol <- round_any(robot_table$vol_needed_for_PCR, 0.5)
    robot_table$Water_Vol <- round_any(robot_table$water_volume_up_PCR, 0.5)
    robot_table$Destination <- paste("Normalized", robot_table$BarcodePlate, sep = "_")
    robot_table$DestinationWell <- unlist(lapply(robot_table$BarcodeWell, well_parser))
    robot_table$WaterSource <- 'WaterSource'
    robot_table$WaterWell <- 1
    robot_table$NormalizedVolume <- robot_table$DNA_Vol + robot_table$Water_Vol
    
    output <- as.data.frame(select(robot_table, WaterSource, WaterWell, DNASource, DNASourceWell, Destination, DestinationWell, DNA_Vol, Water_Vol, Warning, BarcodeID, PlateID, SampleWell, NormalizedVolume, StartingConc, FinalConc))
    
    
  }
  output <- output %>% arrange(PlateID)
  
  write.csv(output, output_name, row.names = FALSE)
  
  return(output)
  
  
}