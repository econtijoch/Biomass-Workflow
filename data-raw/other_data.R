other_data_raw_filename <- system.file("extdata", "Sample_Other_Data.csv", package = "BiomassWorkflow")

Other_Data <- readr::read_csv(other_data_raw_filename)

devtools::use_data(Other_Data, overwrite = TRUE)
