#' Wet vs Dry Weight calculation
#'
#' @param empty_weights file with empty weights of tubes
#' @param full_weights file with weights of tubes with sample in them
#' @param cooked_weights file with weights of tubes after cooking at 105C overnight
#' @param mapping file to add metadata to samples
#'
#' @return data frame with wet and dry weight information
#' @export

wet_dry_weight <- function(empty_weights, full_weights, cooked_weights, mapping = NULL) {
  
  empty <- readr::read_delim(empty_weights, delim = '\t', col_names = c("BarcodeID", "Empty Weight", "Empty Date", "Empty Time"))
  full <-  readr::read_delim(full_weights, delim = '\t', col_names = c("BarcodeID", "Full Weight", "Full Date", "Full Time"))
  cooked <-  readr::read_delim(cooked_weights, delim = '\t', col_names = c("BarcodeID", "Dried Weight", "Dried Date", "Dried Time"))
  
  dry_weight_data <- empty %>% dplyr::left_join(.,full, by = 'BarcodeID') %>% dplyr::left_join(., cooked, by = 'BarcodeID') %>% dplyr::mutate(`Pellet Weight` = `Full Weight` - `Empty Weight`, `Dry Weight` = `Dried Weight` - `Empty Weight`, `Water Percentage` = 100*(`Pellet Weight` - `Dry Weight`)/`Pellet Weight`, `Dry Percentage` = 100 - `Water Percentage`) %>% dplyr::select(BarcodeID, `Pellet Weight`, `Dry Weight`, `Water Percentage`, `Dry Percentage`)
  
  if (!is.null(mapping)) {
    data_map <- readr::read_csv(mapping)
    dry_weight_data <- dplyr::left_join(dry_weight_data, data_map)
  }
  
  return(dry_weight_data)
}