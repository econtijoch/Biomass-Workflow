#' Helper function to pull in fonts for use with plotting themes
#'
#' @param libname library name
#' @param pkgname package name
#'
#' @return nothing
#' @export

.onAttach <- function(libname, pkgname) {
  pdfFonts <- grDevices::pdfFonts
  suppressWarnings(suppressMessages(extrafont::font_import(pattern = "*Arial", prompt = F)))
  extrafont::loadfonts(device = "pdf", quiet = T)
  }
