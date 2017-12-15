#' Raw Fluorescence data for the plate measured with Broad-Range dye
#'
#' A dataset that contains the information from a plate reader file, and is used to re-create a raw fluorescence file as if it came from the plate reader. This data is used in the sample_file_maker script to generate sample raw files.
#'
#' @docType data
#' @format This data is not in a standard format since it is simply the data read in directly from an excel file.
#' @source Generated in lab.
#' @name BR_samples_raw
"BR_samples_raw"

#' Mapping file for the plate containing Broad-Range dye standard curve
#'
#' A dataset that contains the information from a mapping, and is used to re-create a template mapping file. This data is used in the sample_file_maker script to generate a sample mapping file. This dataset contains an example of how to include standards for a standard curve
#' 
#' @docType data
#' @format A table with 96 rows and 10 columns
#' @source Generated in lab for the purposes of analyzing this type of data
#' @name BR_standard_mapping
"BR_standard_mapping"

#' Raw Fluorescence data for the plate measured with High-Sensitivity dye
#'
#' A dataset that contains the information from a plate reader file, and is used to re-create a raw fluorescence file as if it came from the plate reader. This data is used in the sample_file_maker script to generate sample raw files.
#'
#' @docType data
#' @format This data is not in a standard format since it is simply the data read in directly from a .csv file.
#' @source Generated in lab.
#' @name HS_samples_raw
"HS_samples_raw"

#' Mapping file for the plate containing High-Sensitivity dye standard curve
#'
#' A dataset that contains the information from a mapping, and is used to re-create a template mapping file. This data is used in the sample_file_maker script to generate a sample mapping file. This dataset contains an example of how to include standards for a standard curve
#' 
#' @docType data
#' @format A table with 96 rows and 10 columns
#' @source Generated in lab for the purposes of analyzing this type of data
#' @name HS_standard_mapping
"HS_standard_mapping"


#' Empty tube weight file
#'
#' A file that contains the empty tube weights of tubes used in experiment - scanned for tube barcode and BarcodeID for linking with mapping file
#' 
#' @docType data
#' @format tab-delimited text file
#' @source Generated in lab for the purposes of analyzing this type of data
#' @name Empty_weights
"Empty_weights"


#' Full tube weight file
#'
#' A file that contains the full tube weights of tubes used in experiment - scanned for tube barcode for linking with empty weights file
#' 
#' @docType data
#' @format tab-delimited text file
#' @source Generated in lab for the purposes of analyzing this type of data
#' @name Full_weights
"Full_weights"


#' Mapping file for samples in microbial density experiment
#'
#' A dataset that contains the information from a mapping, and is used to re-create a template mapping file. This data is used in the sample_file_maker script to generate a sample mapping file
#'
#' @docType data
#' @format A table with 96 rows and 21 columns
#' @source Generated in lab for the purposes of analyzing this type of data
#' @name mapping_file
"mapping_file"


#' Sample Info for samples in microbial density experiment
#'
#' A dataset that contains the information used to create a mapping file
#'
#' @docType data
#' @format A table with 96 rows and 21 columns
#' @source Generated in lab for the purposes of analyzing this type of data
#' @name Sample_info
"Sample_info"


#' Raw Fluorescence data for the plate containing standards
#'
#' A dataset that contains the information from a plate reader file, and is used to re-create a raw fluorescence file as if it came from the plate reader. This data is used in the sample_file_maker script to generate sample raw files.
#'
#' @docType data
#' @format This data is not in a standard format since it is simply the data read in directly from a .csv file.
#' @source Generated in lab.
#' @name Standards_raw
"Standards_raw"


#' Tube Order from 96-well format barcode scanner (.csv)
#'
#' A dataset that contains the location of tubes used in experiment
#'
#' @docType data
#' @format A table with up to 96 rows
#' @source Generated in lab for the purposes of analyzing this type of data
#' @name Tube_order
"Tube_order"



#' Fully processed other data (.csv)
#'
#' A dataset that contains processed data from othere experiments
#'
#' @docType data
#' @format Table with many samples
#' @source Generated in lab for the purposes of analyzing this type of data
#' @name Other_Data
"Other_Data"