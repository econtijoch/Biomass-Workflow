#' Function to process 16S sequencing data and scale individual samples by biomass, generating absolute abundances
#' @param biom_file .biom table in rhdf5 format. It is wise to use a pre-filtered table (for example, de-duplicated)
#' @param metadata_file the mapping file used in QIIME to split your sequencing library (must include biomass data)
#' @param taxonomy_file a file containing taxonomy (e.g. output from assign_taxonomy.py in QIIME, or greengenes taxonomy)
#' @param filter OPTIONAL: minimum relative abundance to filter from data
#' @return a list containing OTU tables with and without taxonomy (at each given depth), both scaled and unscaled (the output is a list of lists). 
#' @export
#'

individual_scale <- function(biom_file, metadata_file, taxonomy_file, filter) {
    
    cat("Reading Biom Table...\n")
    # Read in biom table, begin manipulating
    raw_biom_table <- biom::read_biom(biom_file)
    biom_data <- methods::as(biom::biom_data(raw_biom_table), "matrix")
    biom_only <- as.data.frame(t(biom_data))
    colnames(biom_only) <- paste("OTU", colnames(biom_only), sep = "_")
    biom_only$X.SampleID <- as.character(row.names(biom_only))
    row.names(biom_only) <- NULL
    
    cat("Reading Metadata...\n")
    # Read in metadata, QC for NA values of biomass
    metadata <- utils::read.delim(metadata_file)
    if ("biomass_ratio" %in% colnames(metadata)) {
        metadata$X.SampleID <- as.character(metadata$X.SampleID)
        metadata <- metadata[!is.na(metadata$biomass_ratio), ]
    } else {
        stop("Biomass data not in metadata file")
    }
    
    
    cat("Making sure OTU data and metadata are properly aligned...\n")
    # Make sure we are working with the right data, sort in same order:
    biom_only <- biom_only[biom_only$X.SampleID %in% base::intersect(biom_only$X.SampleID, metadata$X.SampleID), ] %>% 
        dplyr::arrange(X.SampleID)
    metadata <- metadata[metadata$X.SampleID %in% base::intersect(biom_only$X.SampleID, metadata$X.SampleID), ] %>% 
        dplyr::arrange(X.SampleID)
    
    cat("Reading Taxonomy...\n")
    # Read in Taxonomy
    taxonomy <- utils::read.delim(taxonomy_file, header = F)
    taxonomy <- as.data.frame(apply(taxonomy, 2, function(x) gsub("\\s+", "", x)))
    colnames(taxonomy) <- c("OTU_ID", "Taxon")
    taxonomy$OTU_ID <- paste("OTU", taxonomy$OTU_ID, sep = "_")
    taxonomy$OTU_ID <- as.character(taxonomy$OTU_ID)
    
    cat("Manipulating data...\n")
    
    # Add metadata biom_merged <- inner_join(biom_only, biomass, by = 'X.SampleID')
    
    # Work with specific components
    otus_only <- biom_only[, -length(names(biom_only))]
    # metadata_only <- biom_merged[, !grepl('OTU_', names(biom_merged))] metadata_only$X.SampleID <-
    # as.character(metadata_only$X.SampleID)
    
    cat("Scaling taxonomic abundances...\n")
    # Create relative abundances and scale
    pre_normalized <- otus_only/base::rowSums(otus_only)
    
    if (missing(filter)) {
        cat("OTU table is NOT being filtered by relative abundance...\n")
        normalized <- pre_normalized
    } else {
        cat(paste("OTU table is being filtered to remove OTUs with relative abundance below", filter, "...\n", sep = " "))
        max_otu_fraction <- apply(pre_normalized, 2, max)
        normalized <- pre_normalized[, max_otu_fraction > filter]
    }
    if (ncol(normalized) == 0) {
        stop("Filter is too strict, all OTUs filtered out!")
    }
    scaled <- normalized * metadata$biomass_ratio
    relative <- normalized * 100
    scaled$X.SampleID <- as.character(metadata$X.SampleID)
    relative$X.SampleID <- as.character(metadata$X.SampleID)
    
    fraction_filtered <- 1 - (base::ncol(normalized)/base::ncol(pre_normalized))
    cat(paste("OTU table successfully filtered (", round(fraction_filtered, 3) * 100, "% of OTUs removed)...\n", sep = ""))
    
    
    
    # Re-join metadata (may not be necessary to have added previously, but am concerned about not applying correct
    # biomass scaling to samples)
    otus_scaled <- dplyr::left_join(metadata, scaled, by = "X.SampleID")
    otus_relative <- dplyr::left_join(metadata, relative, by = "X.SampleID")
    
    # Melt data to help create plots
    melted <- reshape2::melt(data = scaled, id.vars = "X.SampleID", measure.vars = colnames(scaled)[-length(colnames(scaled))])
    # QC to elimnate errors and spurious negative abundance (spot-checked, this occurs only once from all samples, and
    # is minimally negative anyways...)
    if (length(melted[melted$value < 0, ]$value) > 0) {
        melted[melted$value < 0, ]$value <- 0
    }
    names(melted)[names(melted) == "variable"] <- "OTU_ID"
    melted$OTU_ID <- as.character(melted$OTU_ID)
    
    relative_melted <- reshape2::melt(data = relative, id.vars = "X.SampleID", measure.vars = colnames(relative)[-length(colnames(relative))])
    names(relative_melted)[names(relative_melted) == "variable"] <- "OTU_ID"
    relative_melted$OTU_ID <- as.character(relative_melted$OTU_ID)
    
    cat("Adding taxonomy and collapsing OTUs by taxonomy...\n")
    # Add taxonomy
    taxonomy_added <- dplyr::inner_join(melted, taxonomy, by = "OTU_ID")
    relative_taxonomy_added <- dplyr::inner_join(relative_melted, taxonomy, by = "OTU_ID")
    
    # Collapse taxonomy
    taxonomy_collapsed <- taxonomy_added %>% dplyr::group_by(X.SampleID, Taxon) %>% dplyr::summarize(absolute_abundance = sum(value))
    relative_taxonomy_collapsed <- relative_taxonomy_added %>% dplyr::group_by(X.SampleID, Taxon) %>% dplyr::summarize(relative_abundance = sum(value))
    
    ## Make tables at different levels of taxonomic depth
    
    phylogeny <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    
    # Split taxonomy
    split_tax <- taxonomy_collapsed %>% tidyr::separate(Taxon, phylogeny, sep = ";.__", remove = F, fill = "right")
    split_tax$Kingdom <- "Bacteria"
    relative_split_tax <- relative_taxonomy_collapsed %>% tidyr::separate(Taxon, phylogeny, sep = ";.__", remove = F, 
        fill = "right")
    relative_split_tax$Kingdom <- "Bacteria"
    
    melted_by_tax_scaled <- list()
    compact_by_tax_scaled <- list()
    melted_by_tax_relative <- list()
    compact_by_tax_relative <- list()
    
    cat("Creating melted and compact tables by taxonomic depth...\n")
    for (i in 1:length(phylogeny)) {
        
        # Define a label for plotting
        label <- paste(phylogeny[i - 1], phylogeny[i], sep = "__")
        # Identify columns to add
        dots <- lapply(phylogeny[1:i], as.symbol)
        
        # Create table that collapses split taxonomy table by each depth, add label
        table <- split_tax %>% dplyr::group_by(X.SampleID) %>% dplyr::group_by_(.dots = dots, add = T) %>% dplyr::summarise(abundance = sum(absolute_abundance))
        if (i > 1) {
            table$short_label <- paste(table[[phylogeny[i - 1]]], table[[phylogeny[i]]], sep = "__")
            table <- table %>% tidyr::unite_("long_label", phylogeny[1:i], sep = "__", remove = F)
        } else {
            table$short_label <- table[[phylogeny[i]]]
            table$long_label <- table[[phylogeny[i]]]
        }
        # Add info to table
        table$Abundance_Type <- "Absolute"
        table$Depth <- phylogeny[i]
        
        # Add to output list
        melted_by_tax_scaled[[phylogeny[i]]] <- table
        
        # Create compact version
        pre_compact_scaled <- reshape2::dcast(table, X.SampleID + Depth + Abundance_Type ~ long_label, value.var = "abundance")
        compact_by_tax_scaled[[phylogeny[i]]] <- dplyr::left_join(metadata, pre_compact_scaled, by = "X.SampleID")
        
        
        ## Relative Abundance
        
        rel_table <- relative_split_tax %>% dplyr::group_by(X.SampleID) %>% dplyr::group_by_(.dots = dots, add = T) %>% 
            dplyr::summarise(abundance = sum(relative_abundance))
        if (i > 1) {
            rel_table$short_label <- paste(rel_table[[phylogeny[i - 1]]], rel_table[[phylogeny[i]]], sep = "__")
            rel_table <- rel_table %>% tidyr::unite_("long_label", phylogeny[1:i], sep = "__", remove = F)
        } else {
            rel_table$short_label <- rel_table[[phylogeny[i]]]
            rel_table$long_label <- rel_table[[phylogeny[i]]]
        }
        rel_table$Abundance_Type <- "Relative"
        rel_table$Depth <- phylogeny[i]
        
        melted_by_tax_relative[[phylogeny[i]]] <- rel_table
        
        pre_compact_relative <- reshape2::dcast(rel_table, X.SampleID + Depth + Abundance_Type ~ long_label, value.var = "abundance")
        compact_by_tax_relative[[phylogeny[i]]] <- dplyr::left_join(metadata, pre_compact_relative, by = "X.SampleID")
    }
    
    
    return(list(scaled_otus = otus_scaled, relative_otus = otus_relative, melted_scaled_by_taxonomy = melted_by_tax_scaled, 
        melted_relative_by_taxonomy = melted_by_tax_relative, compact_scaled_by_taxonomy = compact_by_tax_scaled, compact_relative_by_taxonomy = compact_by_tax_relative, metadata = metadata))
    
}
