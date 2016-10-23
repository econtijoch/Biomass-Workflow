#' Function to generate sample data files to work wtih locally
#' @param directory If given, a directory into which to create files
#' @return A collection of files useful for getting started with the BiomassWorkflow package
#' @export
#'

sample_file_maker <- function(directory = getwd()) {
  starting <- getwd()
  if (file.exists(directory)) {
    setwd(directory)
  } else {
    dir.create(directory)
    setwd(directory)
  }
  
  # Make excel file with plate 1
  workbook <- XLConnect::loadWorkbook(paste(directory, "SamplePlateReader_Plate1.xlsx", sep = "/"), create = TRUE)
  XLConnect::createSheet(workbook, name = "Sheet1")
  XLConnect::writeWorksheet(workbook, plate1_raw, sheet = "Sheet1")
  XLConnect::saveWorkbook(workbook)
  
  # Make .csv file with plate 2
  utils::write.csv(plate2_raw, file = paste(directory, "SamplePlateReader_Plate2.csv", sep = "/"), row.names = F)
  
  # Make .csv file with mapping 1
  utils::write.csv(map1, file = paste(directory, "SampleMapping_Plate1.csv", sep = "/"), row.names = F)
  
  # Make .csv file with mapping 2
  utils::write.csv(map2, file = paste(directory, "SampleMapping_Plate2.csv", sep = "/"), row.names = F)
  
  setwd(starting)
}
