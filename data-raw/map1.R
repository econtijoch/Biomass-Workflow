plate1_mapfilename <- system.file("extdata", "SampleMapping_Plate1.csv", package = "BiomassWorkflow")

map1 <- utils::read.csv(plate1_mapfilename)

devtools::use_data(map1, overwrite = TRUE)
