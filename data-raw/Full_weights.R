Full_weights_filename <- system.file("extdata", "Sample_Full_Weights.txt", package = "BiomassWorkflow")

Full_weights <- utils::read.delim(Full_weights_filename, delim = '\t')

devtools::use_data(Full_weights, overwrite = TRUE)
