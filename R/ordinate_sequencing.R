#' Ordination of scaled sequencing object
#'
#' @param sequencing_object output of individual_scale function
#' @param method method for ordination
#'
#' @return a list of outputs containing ordination results and first 10 PC's
#' @export
#'

ordinate_sequencing <-
  function(sequencing_object, method = 'euclidean') {
    metadata <- sequencing_object[['sample_metadata']]
    
    relative_otus <- sequencing_object[['relative_otus']]
    absolute_otus <- sequencing_object[['scaled_otus']]
    
    relative_data <-
      relative_otus %>% dplyr::select(X.SampleID, dplyr::starts_with("OTU_")) %>% t() %>% as.data.frame()
    colnames(relative_data) <- t(relative_data)[, 1]
    rownames(relative_data) <-
      unlist(strsplit(rownames(relative_data), "_"))[c(T, F)]
    relative_matrix <- t(as.matrix(relative_data[-1,]))
    class(relative_matrix) <- 'numeric'
    
    absolute_data <-
      absolute_otus %>% dplyr::select(X.SampleID, dplyr::starts_with("OTU_")) %>% t() %>% as.data.frame()
    colnames(absolute_data) <- t(absolute_data)[, 1]
    rownames(absolute_data) <-
      unlist(strsplit(rownames(absolute_data), "_"))[c(T, F)]
    absolute_matrix <- t(as.matrix(absolute_data[-1,]))
    class(absolute_matrix) <- 'numeric'
    if (min(absolute_matrix) < 0) {
      absolute_matrix[absolute_matrix < 0] <- 0
    }
    
    distance_table_relative <-
      as.matrix(vegan::vegdist(relative_matrix, method = method))
    pc_table_relative <-
      stats::cmdscale(
        distance_table_relative,
        k = nrow(relative_matrix) - 1,
        eig = TRUE,
        add = TRUE
      )
    
    
    distance_table_absolute <-
      as.matrix(vegan::vegdist(absolute_matrix, method = method))
    pc_table_absolute <-
      stats::cmdscale(
        distance_table_absolute,
        k = nrow(absolute_matrix) - 1,
        eig = TRUE,
        add = TRUE
      )
    
    
    pc_only_relative <-
      as.data.frame(pc_table_relative$points[, 1:10])
    colnames(pc_only_relative) <-
      paste("PC", 1:10, sep = "")
    pc_only_relative$X.SampleID <-
      as.character(row.names(relative_matrix))
    pc_only_relative$Shannon <-
      vegan::diversity(relative_matrix, index = 'shannon')
    pc_only_relative$Simpson <-
      vegan::diversity(relative_matrix, index = 'simpson')
    pc_output_relative <-
      dplyr::left_join(pc_only_relative, metadata, by = 'X.SampleID')
    pc_output_relative <- droplevels.data.frame(pc_output_relative)
    
    
    
    
    
    pc_only_absolute <-
      as.data.frame(pc_table_absolute$points[, 1:10])
    colnames(pc_only_absolute) <-
      paste("PC", 1:10, sep = "")
    pc_only_absolute$X.SampleID <-
      as.character(row.names(absolute_matrix))
    pc_only_absolute$Shannon <-
      vegan::diversity(absolute_matrix, index = 'shannon')
    pc_only_absolute$Simpson <-
      vegan::diversity(absolute_matrix, index = 'simpson')
    pc_output_absolute <-
      dplyr::left_join(pc_only_absolute, metadata, by = 'X.SampleID')
    
    pc_output_absolute <- droplevels.data.frame(pc_output_absolute)
    
    
    return(
      list(
        absolute_ordination_output = pc_table_absolute,
        relative_ordination_output = pc_table_relative,
        absolute_plotting_data = pc_output_absolute,
        relative_plotting_data = pc_output_relative,
		absolute_distance_matrix = distance_table_absolute,
		relative_distance_matrix = distance_table_relative
      )
    )
    
  }
