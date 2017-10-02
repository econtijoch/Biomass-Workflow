HS_standard_mapping_filename <- system.file("extdata", "Sample_HS_Standards_Mapping.csv", package = "BiomassWorkflow")

HS_standard_mapping <- utils::read.csv(HS_standard_mapping_filename)

devtools::use_data(HS_standard_mapping, overwrite = TRUE)
