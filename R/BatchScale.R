#' Scale taxonomy summary plots (output of summarize_taxa_through_plots.py of QIIME) by biomass
#'
#' @param biomass_file input file that contains biomass data for each x-axis category of your summary plots
#' @param taxonomy_directory path to directory that contains output of the QIIME funciton call
#' @param OutputPrefix OPTIONAL: a string to append to the beginning of output files (default = 'Scaled')
#' @param XAxisLabel OPTIONAL: a string to label x-axis of plots (default = 'Timepoint')
#' @param YAxisLabel OPTIONAL: a string to label y-axis of plots (default = 'ug DNA per mg Fecal Pellet')
#' @param OutputDirectory OPTIONAL: path to output directory (by default './')
#' @param batchName OPTIONAL: string to label batch of plots (default = 'Sample')
#' @param ... Optional arguments
#' @return a list that contains within it: a list of the scale taxonomy plots (at the various levels), and the scaled plots
#' @export
#'



BatchScale <- function(biomass_file, taxonomy_directory, OutputPrefix = "Scaled", XAxisLabel = "Timepoint", YAxisLabel = "ug DNA per mg Fecal Pellet", 
    OutputDirectory = ".", batchName = "Sample", ...) {
    
    
    # Read in biomass data for x variable, convert entries to numeric format, clean up
    biomass <- utils::read.csv(biomass_file)
    biomass$Biomass <- as.numeric(as.character(biomass$Biomass))
    
    xvariables <- as.character(biomass[, 1])
    
    transpose <- as.data.frame(t(biomass))[-1, ]
    colnames(transpose) <- xvariables
    
    # Load raw taxonomy files
    files <- list.files(taxonomy_directory, pattern = "L[2-6].txt")
    
    # Create empty master list and figures output list
    master_taxonomy_scaled <- data.frame()
    figures <- list()
    
    # Make directory for output files
    dir.create(OutputDirectory)
    
    # Iterate over taxonomy levels
    for (file in files) {
        
        # Read taxonomy level and rename as appropriate
        raw_level = substr(file, nchar(file) - 5, nchar(file) - 4)
        
        if (raw_level == "L2") {
            level = "Phylum"
        } else if (raw_level == "L3") {
            level = "Class"
        } else if (raw_level == "L4") {
            level = "Order"
        } else if (raw_level == "L5") {
            level = "Family"
        } else if (raw_level == "L6") {
            level = "Genus"
        }
        
        # Read in abundance information
        input_taxonomy_data <- utils::read.table(paste(taxonomy_directory, file, sep = ""), sep = "\t", header = TRUE)
        
        
        # Create new table for scaled values
        output_taxonomy_data <- input_taxonomy_data
        
        # (Double)Check to see that x-axis values are the same (for the case that they are not indicated in the function
        # call)
        checkpoint = colnames(input_taxonomy_data)[2:length(input_taxonomy_data)] == colnames(transpose)
        if (!all(checkpoint)) {
            print("The x-axis variable names given in the biomass data file do not match up with the x-axis variable names given in the taxonomy data files. Please check your mapping files or input files to fix this error.")
        } else {
            
            # Iterate over x-axis variable (e.g. conditions, timepoints, etc.)
            for (xvariable in xvariables) {
                
                # Scale abundance information by factor of biomass
                biomass_scale <- as.numeric(levels(transpose[, xvariable])[1])
                output_taxonomy_data[, xvariable] <- biomass_scale * input_taxonomy_data[, xvariable]
            }
        }  # Close iteration over x variables
        
        # Add taxonomic information to the processed output table (for identification in master table)
        output_taxonomy_data$level <- level
        
        # Combine information into a master table (for output)
        master_taxonomy_scaled <- rbind(master_taxonomy_scaled, output_taxonomy_data)
        
        # Create individual scaled output files
        output_data_filename <- paste(OutputPrefix, batchName, level, "Level_Data.csv", sep = "_")
        solo_table <- output_taxonomy_data[, !(names(output_taxonomy_data) %in% "level")]
        utils::write.table(solo_table, file = paste(OutputDirectory, output_data_filename, sep = "/"), sep = ",", row.names = FALSE)
        
        # Create stacked bar graph for each taxon, combine into the figure list
        melted_output <- reshape2::melt(solo_table, id = "Taxon")
        figures[[level]] <- ggplot2::ggplot(data = figures[[level]], ggplot2::aes_string(x = "variable", y = "value", 
            fill = "Taxon")) + ggplot2::geom_bar(color = "black", stat = "identity") + EJC_theme() + ggplot2::scale_fill_manual(values = EJC_colors) + 
            ggplot2::guides(fill = ggplot2::guide_legend(ncol = 5, reverse = T)) + ggplot2::labs(fill = "Taxon", y = YAxisLabel, 
            x = XAxisLabel, title = paste(level, "Level Absolute Abundance", sep = " "))
        
        # Create PDF for figure, with legend on a seperate page
        output_figure_filename <- paste(OutputPrefix, batchName, level, "Level_Figure.pdf", sep = "_")
        cowplot::save_plot(filename = output_figure_filename, plot = figures[[level]], base_width = 5, base_height = 6)
    }  # Close iteration over taxonomy levels
    
    # Create an output with the master scaled list and figures
    output <- list(master_taxonomy_scaled, figures)
    return(output)
}
