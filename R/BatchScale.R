BatchScale <- function(biomass_file, taxonomy_directory, ...) {
 
  # Parse inputs and override default values if given
  args = list(...)
  OutputPrefix = "Scaled"
  XAxisLabel = "Timepoint"
  YAxisLabel = 'ug DNA per mg Fecal Pellet'
  OutputDirectory = "."
  
  if (!is.null(args$OutputPrefix)) {
  OutputPrefix = args$OutputPrefix
  }
  
  if (!is.null(args$XAxisLabel)) {
    XAxisLabel = args$XAxisLabel
  }
  
  if (!is.null(args$YAxisLabel)) {
    YAxisLabel = args$YAxisLabel
  }
  
  if(!is.null(args$XAxisValues)) {
    XAxisValues = args$XAxisValues
    xvariables = XAxisValues
  }
  
  if(!is.null(args$batchName)) {
    batchName = args$batchName
  }
  
  if(!is.null(args$OutputDirectory)) {
    OutputDirectory = args$OutputDirectory
  }
  
    
  # Read in biomass data for x variable, convert entries to numeric format, clean up
  biomass <- read.csv(biomass_file)
  biomass$Biomass <- as.numeric(as.character(biomass$Biomass))
  
  # Resort to X-axis values in biomass file if none given to function
  if(is.null(args$XAxisValues)) {
    xvariables <- as.character(biomass[,1])
  }
  
  transpose <- as.data.frame(t(biomass))[-1,]
  colnames(transpose) <- xvariables
  
  # Load raw taxonomy files
  files <- list.files(taxonomy_directory, pattern = 'L[2-6].txt')
  
  # Create empty master list and figures output list
  master_taxonomy_scaled <- data.frame()
  figures <- list()
  
  # Make directory for output files
  dir.create(OutputDirectory)
  
  # Iterate over taxonomy levels
  for (file in files) {
    
    # Read taxonomy level and rename as appropriate
    raw_level = substr(file, nchar(file) - 5, nchar(file) - 4)
    
    if (raw_level == 'L2') {
      level = 'Phylum'
    } else if (raw_level == 'L3') {
      level = 'Class'
    } else if (raw_level == 'L4') {
      level = 'Order'
    } else if (raw_level == 'L5') {
      level = 'Family'
    } else if (raw_level == 'L6') {
      level = 'Genus'
    }
    
    # Read in abundance information
    input_taxonomy_data <- read.table(paste(taxonomy_directory, file, sep = ""), header = TRUE)
    
    # Apply new x axis values, if given
    if (!is.null(args$XAxisValues)){
      colnames(input_taxonomy_data) = c("Taxon", XAxisValues)
    }
    
    # Create new table for scaled values
    output_taxonomy_data <- input_taxonomy_data
    
    # (Double)Check to see that x-axis values are the same (for the case that they are not indicated in the function call)
    checkpoint = colnames(input_taxonomy_data)[2:length(input_taxonomy_data)] == colnames(transpose)
    if (!all(checkpoint)) {
      print("The x-axis variable names given in the biomass data file do not match up with the x-axis variable names given in the taxonomy data files. Please check your mapping files or input files to fix this error.")
    } else {
    
    # Iterate over x-axis variable (e.g. conditions, timepoints, etc.)
    for (xvariable in xvariables) {
    
    # Scale abundance information by factor of biomass
    biomass_scale <- as.numeric(levels(transpose[,xvariable])[1])
    output_taxonomy_data[,xvariable] <- biomass_scale * input_taxonomy_data[,xvariable]
      }
    } # Close iteration over x variables
    
    # Add taxonomic information to the processed output table (for identification in master table)
    output_taxonomy_data$level <- level
    
    # Combine information into a master table (for output)
    master_taxonomy_scaled <- rbind(master_taxonomy_scaled, output_taxonomy_data)
    
    # Create individual scaled output files
    output_data_filename <- paste(OutputPrefix, batchName, level, "Level_Data.csv", sep = "_")
    solo_table <- output_taxonomy_data[,!(names(output_taxonomy_data) %in% 'level')]
    write.table(solo_table,file = paste(OutputDirectory, output_data_filename, sep = "/"), sep = ",", row.names = FALSE)
    
    # Create stacked bar graph for each taxon, combine into the figure list
    melted_output <- melt(solo_table, id = "Taxon")
    figures[[level]] <- ggplot2.barplot(data=melted_output, xName="variable", yName = "value", groupName = "Taxon", xtitle= XAxisLabel, ytitle = YAxisLabel)
    
  } # Close iteration over taxonomy levels

  # Create an output with the master scaled list and figures
  output <- list(master_taxonomy_scaled, figures)
  return(output)
}