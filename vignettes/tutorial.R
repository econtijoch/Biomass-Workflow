## ----Dependencies, include=FALSE-----------------------------------------
library(pander)
library(BiomassWorkflow)
library(magrittr)

## ----Setup, eval=FALSE, message=FALSE, warning=FALSE, include=TRUE, results='hide'----
#  # Install and load the devtools package to help download the BiomassWorkflow package
#  install.packages('devtools', repos='http://cran.us.r-project.org')
#  require(devtools)
#  
#  # Install and load the BiomassWorkflow package
#  devtools::install_bitbucket('econtijoch/biomass-workflow')
#  require(BiomassWorkflow)
#  require(magrittr)

## ----SampleData, eval = T, echo=TRUE, message=FALSE, warning=FALSE, results = 'hide', include = TRUE----
# Get and store path to working directory
working_directory <- getwd()
# Generate sample files to this directory
sample_file_maker(directory = working_directory)

## ----mass_and_order, eval=T, fig.path='figure/', message=FALSE, warning=FALSE, include=T, messages=F, results='markup'----

sample_masses <- mass_and_order('Sample_Empty_Weights.txt', 'Sample_Full_Weights.txt', order = 'Sample_Tube_Order_Scanned.csv')


head(sample_masses)

## ----sample_info, echo=TRUE, message=FALSE, warning=FALSE----------------

sample_info <- readr::read_csv('Sample_SampleInfo.csv')

head(sample_info)


## ----mapping_file, echo=TRUE, message=FALSE, warning=FALSE---------------

mapping_file <- dplyr::left_join(sample_masses, sample_info) %>% # Merge sample info and masses
  dplyr::mutate(Type = "Experiment", ReaderWell = SampleWell) %>% # Add our necessary missing components
  dplyr::select(ReaderWell, Type, BarcodeID, SampleMass, everything()) %>%  # This last bit re-orders the mappign file
  dplyr::filter(!is.na(BarcodeID)) # remove wells that didn't have samples (two in this case)

head(mapping_file)



## ----save_mapping--------------------------------------------------------

readr::write_csv(mapping_file, 'Sample_Mapping.csv')


