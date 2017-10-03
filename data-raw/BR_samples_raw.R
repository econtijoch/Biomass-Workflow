BR_samples_raw_filename <- system.file("extdata", "Sample_BR_raw.xls", package = "BiomassWorkflow")

BR_samples_raw <- readxl::read_excel(BR_samples_raw_filename)

devtools::use_data(BR_samples_raw, overwrite = TRUE)

