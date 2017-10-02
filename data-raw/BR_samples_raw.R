BR_samples_raw_filename <- system.file("extdata", "Sample_BR_raw.xls", package = "BiomassWorkflow")

BR_samples_raw <- XLConnect::readWorksheet(object = XLConnect::loadWorkbook(BR_samples_raw_filename), sheet = 1)

devtools::use_data(BR_samples_raw, overwrite = TRUE)
