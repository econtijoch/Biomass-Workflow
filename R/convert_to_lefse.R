#' Convert to lefse-friendly
#'
#' @param table table from individual_scale output (compact_by_taxonomy), at a given taxonomy
#' @param sample string to define column that contains sample ID's
#' @param class string to define column that contains class groupings
#' @param subclass string to define column that contains class groupings (optional)
#' @param depth string defining taxonomic depth to go to (optional - default = 'Species')
#'
#' @return table that you can save as tab delimited file and pipe to LEFSE
#' @export
#'

convert_to_lefse <-
  function(table,
           sample = 'X.SampleID',
           class = 'Condition',
           subclass = NULL,
           depth = "Species") {
    phylogeny_full <-
      c("Kingdom",
        "Phylum",
        "Order",
        "Class",
        "Family",
        "Genus",
        "Species")
    phylogeny <- phylogeny_full[1:grep(depth, phylogeny_full)]

    if (!is.null(subclass)) {
      input <-
        table %>% dplyr::select_(sample, class, subclass, 'dplyr::starts_with("Bacteria")')
      info <- dplyr::select_(input, class, subclass, sample)
      data <-
        dplyr::select_(input, paste0("-",class), paste0("-", subclass), paste0("-", sample)) %>% as.matrix() * 1e5
      rows <- 3
    } else {
      input <-
        table %>% dplyr::select_(sample, class, 'dplyr::starts_with("Bacteria")')
      info <- dplyr::select_(input, class, sample)
      data <-
        dplyr::select_(input, paste0("-",class), paste0("-", sample)) %>% as.matrix() * 1e5
      rows <- 2
    }

    class(data) <- 'integer'

    boosted <-
      dplyr::bind_cols(info, as.data.frame(data))  %>% t() %>% as.data.frame() %>% tibble::rownames_to_column(.) %>% dplyr::mutate(rowname = stringr::str_replace_all(rowname, "__", "|"))
    header <-
      data.frame(sapply(boosted[1:rows, ], function(x) {
        as.character(x)
      }), stringsAsFactors = F)
    colnames(header) <- paste("Col", 1:ncol(header), sep = "")
    data <-
      data.frame(sapply(boosted[-1:-rows, -1], function(x) {
        as.numeric(as.character(x))
      }), stringsAsFactors = F)
    taxa <-
      data.frame(taxonomy = boosted[-rows:-1, 1]) %>% tidyr::separate(
        taxonomy,
        sep = "\\|",
        into = phylogeny_full,
        stringsAsFactors = F
      )
    numeric <- dplyr::bind_cols(taxa, data)

    by_level <- list()
    grouping_table <- numeric %>% dplyr::group_by(Kingdom)
    for (level in phylogeny) {
      grouping_table <- dplyr::group_by_(grouping_table, level, add = T)
      dots <- paste0("-", phylogeny_full)
      by_level[[level]] <-
        grouping_table %>% dplyr::select_(.dots = dots) %>% dplyr::summarise_each(dplyr::funs(sum))
      by_level[[level]] <-
        tidyr::unite_(
          by_level[[level]],
          sep = "|",
          remove = T,
          col = 'Taxonomy',
          phylogeny[phylogeny %in% colnames(by_level[[level]])]
        )
    }
    final_data <-
      data.frame(sapply(dplyr::bind_rows(by_level), function(x) {
        as.character(x)
      }), stringsAsFactors = F)
    colnames(final_data) <- paste("Col", 1:ncol(final_data), sep = "")
    output <- rbind(header, final_data)

    return(output)
  }
