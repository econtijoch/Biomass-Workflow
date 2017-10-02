Tube_order_filename <- system.file("extdata", "Sample_Tube_Order_Scanned.csv", package = "BiomassWorkflow")

Tube_order <- utils::read.csv(Tube_order_filename)

devtools::use_data(Tube_order, overwrite = TRUE)

