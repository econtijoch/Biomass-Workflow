plate1_filename <- system.file("extdata", "SamplePlateReader_Plate1.xlsx", package = "BiomassWorkflow")

plate1_raw <- XLConnect::readWorksheet(object = XLConnect::loadWorkbook(plate1_filename), sheet = 1)

devtools::use_data(plate1_raw, overwrite = TRUE)
