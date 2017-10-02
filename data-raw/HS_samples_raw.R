HS_samples_raw_filename <- system.file("extdata", "Sample_HS_raw.xls", package = "BiomassWorkflow")

HS_samples_raw <- XLConnect::readWorksheet(object = XLConnect::loadWorkbook(HS_samples_raw_filename), sheet = 1)

devtools::use_data(HS_samples_raw, overwrite = TRUE)
