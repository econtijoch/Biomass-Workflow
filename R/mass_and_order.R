#' Generate sample mass and final order of tubes. Will attempt to identify BarcodeID (length 29 generated sample description) and TubeBarcodes (length 10 number found on the Matrix Tubes) in files if possible
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
      readr::read_delim(empty_weights,
                        delim = '\t',
                        col_names = FALSE)
    
    empty_parser <- vector(mode = 'character', length = ncol(empty))
    
    for (column in 1:length(empty_parser)) {
      empty_parser[[column]] <- class(empty[[column]])[[1]]
      if (empty_parser[[column]] == 'hms') {
        empty_parser[[column]] <- "Empty Weight Time"
        empty_parser[[column - 1]] <- "Empty Weight Date"
      
	  } else if (empty_parser[[column]] == 'character') {
        
		if (stringr::str_length(empty[[column]][[1]]) == 10) {
			if ('TubeBarcode' %in% empty_parser) {
				empty_parser[[column]] <- paste0("Column.", column)
			} else {
				empty_parser[[column]] <- 'TubeBarcode'
			}
        } else if (stringr::str_length(empty[[column]][[1]]) == 29) {
		
			if ('BarcodeID' %in% empty_parser) {
				empty_parser[[column]] <- paste0("Column.", column)
			} else {
				empty_parser[[column]] <- 'BarcodeID'
			}
          
        } else {
          empty_parser[[column]] <- paste0("Column.", column)
        }
      } else if (empty_parser[[column]] == 'numeric') {
        empty_parser[[column]] <- 'Empty Mass'
      } else if (empty_parser[[column]] == 'integer') {
		  if (8 < log10(empty[[column]][[1]]) & log10(empty[[column]][[1]]) <= 10) {
			  empty_parser[[column]] <- 'TubeBarcode'
		  } else {
		  empty_parser[[column]] <- paste0("Column.", column)
	  }
      } else {
      	empty_parser[[column]] <- paste0("Column.", column)
      }
      
    }
    colnames(empty) <- empty_parser
    
    
    # Read in full mass file
    full <-
      readr::read_delim(full_weights,
                        delim = '\t',
                        col_names = FALSE)
    
    full_parser <- vector(mode = 'character', length = ncol(full))
    
    for (column in 1:length(full_parser)) {
      full_parser[[column]] <- class(full[[column]])[[1]]
      if (full_parser[[column]] == 'hms') {
        full_parser[[column]] <- "Full Weight Time"
        full_parser[[column - 1]] <- "Full Weight Date"
      } else if (full_parser[[column]] == 'character') {
          if (stringr::str_length(full[[column]][[1]]) == 10) {
  			if ('TubeBarcode' %in% full_parser) {
  				full_parser[[column]] <- paste0("Column.", column)
  			} else {
  				full_parser[[column]] <- 'TubeBarcode'
  			}
          } else if (stringr::str_length(full[[column]][[1]]) == 29) {
  			if ('BarcodeID' %in% full_parser) {
  				full_parser[[column]] <- paste0("Column.", column)
  			} else {
  				full_parser[[column]] <- 'BarcodeID'
  			}
        } else {
          full_parser[[column]] <- paste0("Column.", column)
        }
      } else if (full_parser[[column]] == 'numeric') {
        full_parser[[column]] <- 'Full Mass'
      } else if (full_parser[[column]] == 'integer') {
		  if (8 < log10(full[[column]][[1]]) & log10(empty[[column]][[1]]) <= 10) {
			  full_parser[[column]] <- 'TubeBarcode'
		  } else {
		  full_parser[[column]] <- paste0("Column.", column)
	  }
      } else {
      	full_parser[[column]] <- paste0("Column.", column)
      }
      
    }
    colnames(full) <- full_parser
    
    
    
    # Handle case where order file is provided
    if (!is.null(order)) {
      tube_order <- matrix_plate_parser(order)
      
      # For case where order is provided, compute sample mass and generate output table with order taken into account
      if ('TubeBarcode' %in% colnames(full) &
          'TubeBarcode' %in% colnames(empty)) {
        sample_mass <-
          dplyr::left_join(full, empty, by = 'TubeBarcode') %>% dplyr::mutate(SampleMass = `Full Mass` - `Empty Mass`)
      } else if ('BarcodeID' %in% colnames(full) &
                 'BarcodeID' %in% colnames(empty)) {
        sample_mass <-
          dplyr::left_join(full, empty, by = 'BarcodeID') %>% dplyr::mutate(SampleMass = `Full Mass` - `Empty Mass`)
      } else {
        stop("Error finding TubeBarcode or BarcodeID in one or both input files")
      }
      
      output <-
        dplyr::left_join(tube_order, sample_mass, by = 'TubeBarcode')
      
      
    } else {
      # For case where order is not provided, compute mass and generate output table that is in the order of the full samples
      if ('TubeBarcode' %in% colnames(full) &
          'TubeBarcode' %in% colnames(empty)) {
        output <-
          dplyr::left_join(full, empty, by = 'TubeBarcode') %>% dplyr::mutate(SampleMass = `Full Mass` - `Empty Mass`)
      } else if ('BarcodeID' %in% colnames(full) &
                 'BarcodeID' %in% colnames(empty)) {
        output <-
          dplyr::left_join(full, empty, by = 'BarcodeID') %>% dplyr::mutate(SampleMass = `Full Mass` - `Empty Mass`)
      } else {
        stop("Error finding TubeBarcode or BarcodeID in one or both input files")
      }
    }
    
    # Handle cases where some tubes were not weighed beforehand - use average weight of empty tubes
    if (sum(is.na(output[, "Empty Mass"])) > 0) {
      output[is.na(output$`Empty Mass`), "Empty Weight Date"] <-
        "[WARNING]: Tube not weighed empty. Using average empty tube weight instead."
      output[is.na(output$`Empty Mass`), "Empty Mass"] <-
        mean(output$`Empty Mass`, na.rm = TRUE)
      output <-
        output %>% dplyr::mutate(SampleMass = `Full Mass` - `Empty Mass`)
    }
    
    
    return(output)
  }
