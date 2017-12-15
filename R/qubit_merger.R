#' Function to improve accuracy of DNA measurements for samples measured with both HS and BR dye
#'
#' @param hs_data data frame with HS dye data
#' @param br_data data frame with BR dye data
#'
#' @return data frame with more accurate DNA concentrations and microbial density measurements
#' @export


qubit_merger <- function(hs_data, br_data) {
  hs_data_long <- hs_data %>% dplyr::mutate_(HS_fluorescence = "Fluorescence", HS_dna_concentration = "dna_concentration") %>% dplyr::select_("-Fluorescence", "-dna_concentration", "-microbial_density", "-total_dna", "-Experiment", "-ReaderWell") %>% dplyr::select( -dplyr::contains('vol'), -dplyr::contains('possible'))
  br_data_long <- br_data  %>% dplyr::mutate_(BR_fluorescence = "Fluorescence", BR_dna_concentration = "dna_concentration") %>% dplyr::select_("-Fluorescence", "-dna_concentration", "-microbial_density", "-total_dna", "-Experiment", "-ReaderWell") %>% dplyr::select( -dplyr::contains('vol'), -dplyr::contains('possible'))
  
  combined_data <- dplyr::full_join(hs_data_long, br_data_long)
  
  output_data <- combined_data %>% dplyr::mutate(
    dna_concentration = ifelse(
      test = (HS_dna_concentration < 75 & BR_dna_concentration < 50), yes =  HS_dna_concentration, no = ifelse(
        test = (BR_dna_concentration > 50 & HS_dna_concentration > 75), yes = BR_dna_concentration, no = (HS_dna_concentration + BR_dna_concentration)/2
      )
    ),
    dye_used = ifelse(
      test = (HS_dna_concentration < 75 & BR_dna_concentration < 50), yes =  "HS", no = ifelse(
        test = (BR_dna_concentration > 50 & HS_dna_concentration > 75), yes = "BR", no = "Average"
      )
    ),
    total_dna = dna_concentration*scale_factor*0.1,
    microbial_density = total_dna/SampleMass,
    X16S_possible = (dna_concentration > 1.5 & dna_concentration > 0),
    vol_needed_for_PCR = 400/dna_concentration,
    water_volume_up_PCR = 200 - vol_needed_for_PCR,
    metagenomics_possible =  dna_concentration >= 20,
    vol_needed_for_metagenomics = 600/dna_concentration,
    water_volume_up_metagenomics = 30 - vol_needed_for_metagenomics
  )
  return(output_data)
}
