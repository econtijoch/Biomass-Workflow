plate2_filename <- system.file("extdata", "SamplePlateReader_Plate2.csv", package = "BiomassWorkflow")

plate2_raw <- utils::read.csv(plate2_filename)

devtools::use_data(plate2_raw, overwrite = TRUE)
