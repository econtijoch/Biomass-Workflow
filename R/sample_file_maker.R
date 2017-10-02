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
  utils::data('mapping_file')
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
  BR_raw <- XLConnect::loadWorkbook(paste(directory, "Sample_BR_raw.xls", sep = "/"), create = TRUE)
  XLConnect::createSheet(BR_raw, name = "Sheet1")
  XLConnect::writeWorksheet(BR_raw, BR_samples_raw, sheet = "Sheet1")
  XLConnect::saveWorkbook(BR_raw)
  
  HS_raw <- XLConnect::loadWorkbook(paste(directory, "Sample_HS_raw.xls", sep = "/"), create = TRUE)
  XLConnect::createSheet(HS_raw, name = "Sheet1")
  XLConnect::writeWorksheet(HS_raw, HS_samples_raw, sheet = "Sheet1")
  XLConnect::saveWorkbook(HS_raw)
  
  Std_raw <- XLConnect::loadWorkbook(paste(directory, "Sample_Standards_raw", sep = "/"), create = TRUE)
  XLConnect::createSheet(Std_raw, name = "Sheet1")
  XLConnect::writeWorksheet(Std_raw, Standards_raw, sheet = "Sheet1")
  XLConnect::saveWorkbook(Std_raw)
  
  
  # Make .csv files
  utils::write.csv(BR_standard_mapping, file = paste(directory, "Sample_BR_Standards_Mapping.csv", sep = "/"), row.names = F)
  utils::write.csv(HS_standard_mapping, file = paste(directory, "Sample_HS_Standards_Mapping.csv", sep = "/"), row.names = F)
  utils::write.csv(mapping, file = paste(directory, "Sample_Mapping.csv", sep = "/"), row.names = F)
  utils::write.csv(Sample_info, file = paste(directory, "Sample_SampleInfo.csv", sep = "/"), row.names = F)
  utils::write.csv(Tube_order, file = paste(directory, "Sample_Tube_Order_Scanned.csv", sep = "/"), row.names = F)
  
  # Make .txt files
  utils::write.table(Empty_weights, file = paste(directory, "Sample_Empty_Weights.txt", sep = "/"), row.names = F, sep = '\t')
  utils::write.table(Full_weights, file = paste(directory, "Sample_Full_Weights.txt", sep = "/"), row.names = F, sep = '\t')
  
  setwd(starting)
}
