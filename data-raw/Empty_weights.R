Empty_weights_filename <- system.file("extdata", "Sample_Empty_Weights.txt", package = "BiomassWorkflow")

Empty_weights <- utils::read.delim(Empty_weights_filename, delim = '\t')

devtools::use_data(Empty_weights, overwrite = TRUE)
