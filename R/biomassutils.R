#' biomassutils: A Helper Package for Microbial Biomass Data Analysis
#'
#' This package helps generate and analyze microbiome biomass data, prepare sequencing of microbiome samples, and analye sequencing data.
#'
#' @section Data Generation & Analysis:
#' Functions here
#' @section Sequencing Prep:
#' Functions here
#' @section Sequencing Data Analysis:
#' Functions here
#'
#' @docType package
#' @name biomassutils
NULL

"%ni%" <- Negate("%in%")

if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("X.SampleID", "Taxon", "value", "absolute_abundance", "relative_abundance", "SampleMass", 
        "BarcodeID", "PlateID", "SampleWell", "vol_needed_for_PCR", "water_volume_up_PCR", "vol_needed_for_metagenomics", 
        "water_volume_up_metagenomics", "dna_concentration", "BarcodePlate", "BarcodeWell", "SequencingRun", "WaterSource", 
        "WaterWell", "DNASource", "DNASourceWell", "Destination", "DestinationWell", "DNA_Vol", "Water_Vol", "Warning", 
        "NormalizedVolume", "StartingConc", "FinalConc", "X16S_possible", ".", "SampleID", "BarcodeSequence", "LinkerPrimerSequence", 
        "Read.1", "ReaderWell", "Type", "metagenomics_possible"))  # Work-around NSE
}

