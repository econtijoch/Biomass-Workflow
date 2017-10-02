#' Function to generate sample data files to work wtih locally
#' @param directory If given, a directory into which to create files
#' @return A collection of files useful for getting started with the BiomassWorkflow package
#' @export
#'

sample_file_maker <- function(directory = getwd()) {
  
  utils::data('BR_samples_raw')
  utils::data('HS_samples_raw')
  utils::data('BR_standard_mapping')
  utils::data('HS_standard_mapping')
  utils::data('Empty_weights')
  utils::data('Full_weights')
  utils::data('mapping')
  utils::data('Sample_info')
  utils::data('Standards_raw')
  utils::data('Tube_order')
  
  
  starting <- getwd()
  if (file.exists(directory)) {
    setwd(directory)
  } else {
    dir.create(directory)
    setwd(directory)
  }
  
  # Make excel files
  writexl::write_xlsx(x = BR_samples_raw, path = paste(directory, "Sample_BR_raw.xlsx", sep = "/"))
  writexl::write_xlsx(x = HS_samples_raw, path = paste(directory, "Sample_HS_raw.xlsx", sep = "/"))
  writexl::write_xlsx(x = Standards_raw, path = paste(directory, "Standards_raw.xlsx", sep = "/"))
  
  # Make .csv files
  readr::write_csv(BR_standard_mapping, path = paste(directory, "Sample_BR_Standards_Mapping.csv", sep = "/"))
  readr::write_csv(HS_standard_mapping, path = paste(directory, "Sample_HS_Standards_Mapping.csv", sep = "/"))
  readr::write_csv(mapping, path = paste(directory, "Sample_Mapping.csv", sep = "/"))
  readr::write_csv(Sample_info, path = paste(directory, "Sample_SampleInfo.csv", sep = "/"))
  readr::write_csv(Tube_order, path = paste(directory, "Sample_Tube_Order_Scanned.csv", sep = "/"))
  
  # Make .txt files
  readr::write_delim(Empty_weights, path = paste(directory, "Sample_Empty_Weights.txt", sep = "/"), delim = '\t', col_names = F)
  readr::write_delim(Full_weights, path = paste(directory, "Sample_Full_Weights.txt", sep = "/"), delim = '\t', col_names = F)
  
  setwd(starting)
}
