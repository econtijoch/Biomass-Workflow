#' Function to produce a Beckmann Robot-friendly file and table for diluting samples for metagenomics Sequencing
#' @param dataset an input dataset that includes sample volumes needed for metagenomics, water volume needed for metagenomics, dna concentrations, and sample location information. 
#' @param n_barcode_plates the number of barcode plates available (i.e. if you have 96 unique sequencing barcodes, this would be 1)
#' @return a table contianing the necessary information for the robot to handle the dilution, as well as some QC data for evaluating if there will be any errors. This will also print to terminal the names of the plates necessary (it is useful to have your plates named.)
#' @export
#'

robot_prep_metagenomics <- function(dataset, n_barcode_plates) {
    
    "%ni%" <- Negate("%in%")
    
    sample_number <- nrow(dataset)
    total_number_of_plates_needed <- ceiling(sample_number/96)
    number_of_runs_needed <- ceiling(sample_number/(96 * n_barcode_plates))
    
    dataset$BarcodePlate <- ""
    dataset$BarcodeWell <- ""
    dataset$SequencingRun <- ""
    
    for (k in 1:number_of_runs_needed) {
        if (k < number_of_runs_needed) {
            plates_in_run <- n_barcode_plates
        } else {
            plates_in_run <- total_number_of_plates_needed - (k - 1) * n_barcode_plates
        }
        
        for (i in 1:plates_in_run) {
            for (j in 1:96) {
                entry <- ((i - 1) * 96) + j + (k - 1) * n_barcode_plates * 96
                if (entry <= sample_number) {
                  dataset[entry, "BarcodePlate"] <- paste("Plate", i, sep = "")
                  row <- (j - 1)%/%12
                  column <- j - (12 * (row))
                  row_id <- c("A", "B", "C", "D", "E", "F", "G", "H")
                  dataset[entry, "BarcodeWell"] <- paste(row_id[row + 1], sprintf("%02d", column), sep = "")
                  dataset[entry, "SequencingRun"] <- paste("Metagenomics_Run", k, sep = "")
                }
                
            }
            
        }
        
    }
    
    if ("metagenomics_possible" %in% colnames(dataset)) {
        if (sum(dataset$metagenomics_possible) < nrow(dataset)) {
            cat("WARNING: Running robot prep on samples that have not passed the 'metagenomics possible' check. Re-check data and filter if necessary.\n")
        }
    }
    
    robot_table <- dataset %>% dplyr::select(BarcodeID, PlateID, SampleWell, vol_needed_for_metagenomics, water_volume_up_metagenomics, 
        dna_concentration, BarcodePlate, BarcodeWell, SequencingRun)
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
            robot_table[i, "FinalConc"] <- (robot_table[i, "StartingConc"] * 1)/(25)
            robot_table[i, "TotalDNAin20uL"] <- robot_table[i, "FinalConc"] * 20
        } else if (robot_table[i, "vol_needed_for_metagenomics"] > 25) {
            robot_table[i, "vol_needed_for_metagenomics"] <- 25
            robot_table[i, "water_volume_up_metagenomics"] <- 0
            robot_table[i, "Warning"] <- "[WARNING] DNAvol > 25: Transferred 25 uL + 0 uL Water/EB"
            robot_table[i, "FinalConc"] <- (robot_table[i, "StartingConc"] * 25)/(25)
            robot_table[i, "TotalDNAin20uL"] <- robot_table[i, "FinalConc"] * 20
        } else {
            robot_table[i, "Warning"] <- "25 > DNAvol > 1: Transferred [DNAvol + Water/EB]= 25 uL (Default)"
            robot_table[i, "FinalConc"] <- (robot_table[i, "StartingConc"] * robot_table[i, "vol_needed_for_metagenomics"])/(25)
            robot_table[i, "TotalDNAin20uL"] <- robot_table[i, "FinalConc"] * 20
        }
        
        robot_table$DNASourceWell <- unlist(lapply(robot_table$SampleWell, well_parser))
        
        robot_table$DNA_Vol <- plyr::round_any(robot_table$vol_needed_for_metagenomics, 0.1)
        robot_table$Water_Vol <- plyr::round_any(robot_table$water_volume_up_metagenomics, 0.1)
        robot_table$Destination <- paste("Normalized", robot_table$BarcodePlate, sep = "_")
        robot_table$DestinationWell <- unlist(lapply(robot_table$BarcodeWell, well_parser))
        robot_table$WaterSource <- "WaterSource"
        robot_table$WaterWell <- 1
        robot_table$NormalizedVolume <- robot_table$DNA_Vol + robot_table$Water_Vol
        
        output <- as.data.frame(dplyr::select(robot_table, WaterSource, WaterWell, DNASource, DNASourceWell, Destination, 
            DestinationWell, DNA_Vol, Water_Vol, Warning, BarcodeID, PlateID, SampleWell, SequencingRun, BarcodePlate, 
            BarcodeWell, NormalizedVolume, StartingConc, FinalConc))
        
    }
    output <- output %>% dplyr::arrange(SequencingRun, BarcodePlate, DestinationWell)
    
    cat("Names of plates needed for robot dilution: \n")
    cat(paste(unique(output$PlateID), collapse = "\n"))
    
    
    return(output)
    
    
}
