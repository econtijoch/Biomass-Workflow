mapping_filename <- system.file("extdata", "Sample_Mapping.csv", package = "BiomassWorkflow")

mapping <- utils::read.csv(mapping_filename)

devtools::use_data(mapping, overwrite = TRUE)
