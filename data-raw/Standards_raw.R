Standards_raw_filename <- system.file("extdata", "Sample_Standards_raw.xls", package = "BiomassWorkflow")

Standards_raw <- XLConnect::readWorksheet(object = XLConnect::loadWorkbook(Standards_raw_filename), sheet = 1)

devtools::use_data(Standards_raw, overwrite = TRUE)
