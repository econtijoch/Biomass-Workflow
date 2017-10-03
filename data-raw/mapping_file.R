mapping_filename <- system.file("extdata", "Sample_Mapping.csv", package = "BiomassWorkflow")

mapping_file <- utils::read.csv(mapping_filename)

devtools::use_data(mapping_file, overwrite = TRUE)