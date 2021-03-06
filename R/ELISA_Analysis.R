#' Helper function for IgA Elisa standard curve fitting
#' @param params parameters for model from nls fit
#' @param x data to be fit
#' @return sigmoid model for standards
#' @export

sigmoid_model <- function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}

#' Helper function for IgA Elisa data generation from standard curve parameters
#' @param params parameters for model from nls fit
#' @param y data to be fit
#' @return estimates of IgA based on OD measurements from the fitted model
#' @export


predictor <- function(params, y) {
  params[3] - (log((params[1]/y) - 1)/params[2])
}


#' Function to process Fecal IgA Elisa data
#' @param plate_reader_file the file path of the plate reader file (in .csv or .xls(x) format)
#' @param mapping_file mapping file for plate (for wells with standards, Type must be 'Standard')
#' @param shiny OPTIONAL: necessary for running with shiny app interface since filenames are not the same.
#' @param type OPTIONAL: necessary for running with shiny app, must specify file type
#' @param print OPTIONAL: whether or not to save a copy of the standards curve to the working directory
#' @param upper_asymptote  a in the equation: a/(1 + exp(-b * (x-c))) (starting conditions for nls fit)
#' @param growth_rate b in the equation: a/(1 + exp(-b * (x-c))) (starting conditions for nls fit)
#' @param max_time c in the equation: a/(1 + exp(-b * (x-c))) (starting conditions for nls fit)
#' @param ... optional inputs
#' @return list containing a table of the standards, and the information for the standard curve
#' @export
#'

IgAAnalysis <- function(plate_reader_file, mapping_file, shiny = FALSE, type = NULL, print = FALSE, upper_asymptote = 0.9, growth_rate = 0.5, max_time = 1, ...) {
 
  # Read in files and join
  IgA_raw <- PlateParser(plate_reader_file, shiny, type)
  IgA_map <- ParseMappingFile(mapping_file)
  IgA_data <- dplyr::full_join(IgA_map, IgA_raw)
  
  # Pull out standards, average
  IgA_standards <- IgA_data %>% dplyr::filter(Type == "Standard") %>% dplyr::group_by(BarcodeID, SampleMass) %>% dplyr::summarize(OD = mean(Fluorescence))
  IgA_standards$Standard <- IgA_standards$SampleMass
  IgA_standards$log_std <- log10(IgA_standards$Standard)
  
  # Pull out samples
  IgA_samples <- IgA_data %>% dplyr::filter(Type == "Experiment")
  names(IgA_samples)[names(IgA_samples)=="Fluorescence"] <- "OD"
  
  
  # Make fit model
  x <- IgA_standards$log_std
  y <- IgA_standards$OD
  
  
  fitmodel <- stats::nls(y ~ a/(1 + exp(-b * (x - c))), start=list(a = upper_asymptote, b = growth_rate, c = max_time))
  
  params <- stats::coef(fitmodel)
  
  x_values <- seq(0,3.5, length=1000)
  OD <- sigmoid_model(params,x_values)
  standards_plot <- ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(x = x_values, y = OD)) + ggplot2::geom_point(ggplot2::aes(x = x, y = y)) + ggplot2::labs(x = 'Log[IgA]', y = 'OD', title = 'Standard Curve') + EJC_theme()
  
  IgA_samples$Log_IgA <- predictor(params, IgA_samples$OD)
  IgA_samples$IgA <- (10^IgA_samples$Log_IgA)*IgA_samples$Dilution/1000
  IgA_samples$Sample_Dilution <- paste("Dilution = 1/", IgA_samples$Dilution, sep = "")
  
  
  return(list(data = IgA_samples, standards_plot = standards_plot, standards = IgA_standards))
  
  
  
  
}

#' Function to process ELISA data
#' @param plate_reader_file the file path of the plate reader file (in .csv or .xls(x) format)
#' @param mapping_file mapping file for plate. Must contain: BarcodeID (unique names for samples), Type ("Standard" or "Experiment" to designate type of sample), SampleMass (to hold standards information to generate standard curve)
#' @param shiny OPTIONAL: necessary for running with shiny app interface since filenames are not the same.
#' @param type OPTIONAL: necessary for running with shiny app, must specify file type
#' @param print OPTIONAL: whether or not to save a copy of the standards curve to the working directory
#' @param upper_asymptote  a in the equation: a/(1 + exp(-b * (x-c))) (starting conditions for nls fit)
#' @param growth_rate b in the equation: a/(1 + exp(-b * (x-c))) (starting conditions for nls fit)
#' @param max_time c in the equation: a/(1 + exp(-b * (x-c))) (starting conditions for nls fit)
#' @param log_x_max maximum value of log[Analyte] - used to generate standard curve
#' @param ... optional inputs
#' @return list containing a table of the standards, and the information for the standard curve
#' @export
#'

ElisaAnalysis <- function(plate_reader_file, mapping_file, shiny = FALSE, type = NULL, print = FALSE, upper_asymptote = 0.9, growth_rate = 0.5, max_time = 1, log_x_max = 3.5, ...) {
  
  # Read in files and join
  raw <- PlateParser(plate_reader_file, shiny, type)
  map <- ParseMappingFile(mapping_file)
  combined_data <- dplyr::full_join(map, raw)
  
  # Pull out standards, average
  standards <- combined_data %>% dplyr::filter(Type == "Standard") %>% dplyr::group_by(BarcodeID, SampleMass) %>% dplyr::summarize(OD = mean(Fluorescence))
  standards$Standard <- standards$SampleMass
  standards$log_std <- log10(standards$Standard)
  
  # Pull out samples
  samples <- combined_data %>% dplyr::filter(Type == "Experiment")
  names(samples)[names(samples)=="Fluorescence"] <- "OD"
  
  
  # Make fit model
  x <- standards$log_std
  y <- standards$OD
  
  
  fitmodel <- stats::nls(y ~ a/(1 + exp(-b * (x - c))), start=list(a = upper_asymptote, b = growth_rate, c = max_time))
  
  params <- stats::coef(fitmodel)
  
  x_values <- seq(0, log_x_max, length=1000)
  OD <- sigmoid_model(params,x_values)
  standards_plot <- ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(x = x_values, y = OD)) + ggplot2::geom_point(ggplot2::aes(x = x, y = y)) + ggplot2::labs(x = 'Log[Analyte]', y = 'OD', title = 'Standard Curve') + EJC_theme()
  
  samples$Log_Concentration <- predictor(params, samples$OD)
  samples$Analyte_Concentration <- (10^samples$Log_Concentration)
  
  
  return(list(data = samples, standards_plot = standards_plot, standards = standards))
}
