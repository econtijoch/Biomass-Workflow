BR_standard_mapping_filename <- system.file("extdata", "Sample_BR_Standards_Mapping.csv", package = "BiomassWorkflow")

BR_standard_mapping <- utils::read.csv(BR_standard_mapping_filename)

devtools::use_data(BR_standard_mapping, overwrite = TRUE)
