#' Raw Fluorescence data for the first of two plates in a sample experiment
#'
#' A dataset that contains the information from a plate reader file, and is used to re-create a raw fluorescence file as if it came from the plate reader. This data is used in the sample_file_maker script to generate sample raw files.
#'
#' @format This data is not in a standard format since it is simply the data read in directly from an excel file.
#' @source Generated in lab.
"plate1_raw"

#' Raw Fluorescence data for the second of two plates in a sample experiment
#'
#' A dataset that contains the information from a plate reader file, and is used to re-create a raw fluorescence file as if it came from the plate reader. This data is used in the sample_file_maker script to generate sample raw files.
#'
#' @format This data is not in a standard format since it is simply the data read in directly from a .csv file.
#' @source Generated in lab.
"plate2_raw"

#' Mapping file for the first of two plates in a sample experiment
#'
#' A dataset that contains the information from a mapping, and is used to re-create a template mapping file. This data is used in the sample_file_maker script to generate a sample mapping file
#'
#' @format A table with 96 rows and 10 columns
#' @source Generated in lab for the purposes of analyzing this type of data
"map1"

#' Mapping file for the second of two plates in a sample experiment
#'
#' A dataset that contains the information from a mapping, and is used to re-create a template mapping file. This data is used in the sample_file_maker script to generate a sample mapping file. This dataset contains an example of how to include standards for a standard curve
#'
#' @format A table with 96 rows and 10 columns
#' @source Generated in lab for the purposes of analyzing this type of data
"map2"