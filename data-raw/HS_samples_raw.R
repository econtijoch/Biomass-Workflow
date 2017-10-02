HS_samples_raw_filename <- system.file("extdata", "Sample_HS_raw.xls", package = "BiomassWorkflow")

HS_samples_raw <- readxl::read_excel(HS_samples_raw_filename)

devtools::use_data(HS_samples_raw, overwrite = TRUE)
