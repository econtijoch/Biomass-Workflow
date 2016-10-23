plate2_mapfilename <- system.file("extdata", "SampleMapping_Plate2.csv", package = "BiomassWorkflow")

map2 <- utils::read.csv(plate2_mapfilename)

devtools::use_data(map2, overwrite = TRUE)
