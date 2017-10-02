Full_weights_filename <- system.file("extdata", "Sample_Full_Weights.txt", package = "BiomassWorkflow")

Full_weights <-  readr::read_delim(Full_weights_filename, delim = '\t', col_names = F)

devtools::use_data(Full_weights, overwrite = TRUE)
