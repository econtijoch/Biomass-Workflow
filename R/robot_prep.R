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
  
  robot_table <- dataset %>% select(BarcodeID, Plate, Well, vol_needed_for_PCR, water_volume_up_PCR)
  robot_table$Warning <- "N"
  robot_table$DNASource <- paste("Unnormalized", substring(robot_table$Plate, nchar(robot_table$Plate), nchar(robot_table$Plate)), sep = "")
  
  for (i in 1:nrow(robot_table)) {
    if (robot_table[i, "vol_needed_for_PCR"] < 1) {
      robot_table[i, "vol_needed_for_PCR"] =1
      robot_table[i, "Warning"] = "Y"
    }
    else if (robot_table[i, "vol_needed_for_PCR"] > 40) {
      robot_table[i, "vol_needed_for_PCR"] = 40
      robot_table[i, "Warning"] = "Y"
    }
    
    if (robot_table[i, "water_volume_up_PCR"] < 160) {
      robot_table[i, "water_volume_up_PCR"] = 160
      robot_table[i, "Warning"] = "Y"
    }
    else if (robot_table[i, "water_volume_up_PCR"] > 199) {
      robot_table[i, "water_volume_up_PCR"] = 199
      robot_table[i, "Warning"] = "Y"
    }
    
    robot_table$DNASourceWell <- unlist(lapply(robot_table$Well, well_parser))
    
    robot_table$DNA_Vol <- round_any(robot_table$vol_needed_for_PCR, 0.5)
    robot_table$Water_Vol <- round_any(robot_table$water_volume_up_PCR, 0.5)
    robot_table$Destination <- paste("Normalized", substring(robot_table$DNASource, nchar(robot_table$DNASource), nchar(robot_table$DNASource)), sep = "")
    robot_table$DestinationWell <- robot_table$DNASourceWell
    robot_table$WaterSource <- 'WaterSource'
    robot_table$WaterWell <- 1
    
    output <- as.data.frame(select(robot_table, WaterSource, WaterWell, DNASource, DNASourceWell, Destination, DestinationWell, DNA_Vol, Water_Vol, Warning, BarcodeID, Plate, Well))
    
    
  }
  
  write.csv(output, output_name, row.names = FALSE)
  
  return(output)
  
  
}