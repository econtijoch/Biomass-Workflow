Sample_info_filename <- system.file("extdata", "Sample_SampleInfo.csv", package = "BiomassWorkflow")

Sample_info <- utils::read.csv(Sample_info_filename)

devtools::use_data(Sample_info, overwrite = TRUE)
