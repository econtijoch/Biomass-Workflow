Standards_raw_filename <- system.file("extdata", "Sample_Standards_raw.xls", package = "BiomassWorkflow")

Standards_raw <- readxl::read_excel(path = Standards_raw_filename)

devtools::use_data(Standards_raw, overwrite = TRUE)
