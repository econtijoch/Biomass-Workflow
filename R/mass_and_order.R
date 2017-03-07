#' Generate sample mass and final order of tubes
#'
#' @param empty_weights .txt file with barcodes and weights of empty tubes
#' @param full_weights  .txt file with barcodes and weights of full tubes
#' @param order file from 96 barcode scanner that gives final order of tubes (optional. if not provided, uses the order of tubes in the full tube weight file)
#'
#' @return table of weights and tubes with order of samples
#' @export


mass_and_order <-
  function(empty_weights, full_weights, order = NULL) {
    # Read in Empty mass file
    empty <-
      readr::read_delim(
        empty_weights,
        delim = '\t',
        col_names = c(
          "Barcode",
		  "TubeBarcode",
          "Empty Mass",
          "Empty Weight Date",
          "Empty Weight Time"
        ),
        col_types = list(
          readr::col_character(),
          readr::col_double(),
          readr::col_character(),
          readr::col_character()
        )
      )
	  
    # Read in full mass file
    full <-
      readr::read_delim(
        full_weights,
        delim = '\t',
        col_names = c("Barcode", "TubeBarcode", "Full Mass", "Full Weight Date", "Full Weight Time"),
        col_types = list(
          readr::col_character(),
          readr::col_double(),
          readr::col_character(),
          readr::col_character()
        )
      )
    
    
    # Handle case where order file is provided
    if (!is.null(order)) {
      
      tube_order <- matrix_plate_parser(order)
      
      # For case where order is provided, compute sample mass and generate output table with order taken into account
      sample_mass <-
        dplyr::left_join(full, empty, by = 'Barcode') %>% dplyr::mutate(SampleMass = `Full Mass` - `Empty Mass`)
      
      output <-
        dplyr::left_join(tube_order, sample_mass, by = 'Barcode')
      
      
    } else {
      # For case where order is not provided, compute mass and generate output table that is in the order of the full samples
      output <-
        dplyr::left_join(full, empty, by = 'Barcode') %>% dplyr::mutate(SampleMass = `Full Mass` - `Empty Mass`)
    }
	
	# Handle cases where some tubes were not weighed beforehand - use average weight of empty tubes
	if (sum(is.na(output[,"Empty Mass"])) > 0) {
		output[is.na(output$`Empty Mass`), "Empty Weight Date"] <- "[WARNING]: Tube not weighed empty. Using average empty tube weight instead."
		output[is.na(output$`Empty Mass`), "Empty Mass"] <- mean(output$`Empty Mass`, na.rm = TRUE)
		output <- output %>% dplyr::mutate(SampleMass = `Full Mass` - `Empty Mass`)
	}
	
	
    return(output)
  }
