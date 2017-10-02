Empty_weights_filename <- system.file("extdata", "Sample_Empty_Weights.txt", package = "BiomassWorkflow")

Empty_weights <- readr::read_delim(Empty_weights_filename, delim = '\t', col_names = F)

devtools::use_data(Empty_weights, overwrite = TRUE)
