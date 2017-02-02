#' BiomassWorkflow: A Helper Package for Microbial Biomass Data Analysis
#'
#' This package helps generate and analyze microbiome biomass data, prepare sequencing of microbiome samples, and analye sequencing data.
#'
#' @docType package
#' @name BiomassWorkflow
NULL

"%ni%" <- Negate("%in%")


# Work around for global variable issues in CMD Check
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("X.SampleID", "Taxon", "value", "absolute_abundance", "relative_abundance", "SampleMass", 
        "BarcodeID", "PlateID", "SampleWell", "vol_needed_for_PCR", "water_volume_up_PCR", "vol_needed_for_metagenomics", 
        "water_volume_up_metagenomics", "dna_concentration", "BarcodePlate", "BarcodeWell", "SequencingRun", "WaterSource", 
        "WaterWell", "DNASource", "DNASourceWell", "Destination", "DestinationWell", "DNA_Vol", "Water_Vol", "Warning", 
        "NormalizedVolume", "StartingConc", "FinalConc", "X16S_possible", ".", "SampleID", "BarcodeSequence", "LinkerPrimerSequence", 
        "Fluorescence", "ReaderWell", "Type", "metagenomics_possible", 'plate1_raw', 'plate2_raw', 'map1', 'map2', 'abundance')) 
}

