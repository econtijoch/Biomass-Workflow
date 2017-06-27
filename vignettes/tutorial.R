## ----Dependencies, include=FALSE-----------------------------------------
library(pander)

## ----Setup, eval=T, echo=TRUE, message=FALSE, warning=FALSE, results = 'hide', include = TRUE----
# Install and load the devtools package to help download the BiomassWorkflow package
install.packages('devtools', repos='http://cran.us.r-project.org')
library(devtools)

# Install and load the BiomassWorkflow package
devtools::install_github('econtijoch/Biomass-Workflow')
library(BiomassWorkflow)

## ----SampleData, eval = T, echo=TRUE, message=FALSE, warning=FALSE, results = 'hide', include = TRUE----
# Get and store path to working directory
working_directory <- getwd()
# Generate sample files to this directory
sample_file_maker(directory = working_directory)

## ----DataAnalysis, eval=T, fig.path='figure/', warning=FALSE, include=T, messages = F, results='hide'----

## Plate 1 does not contain standards, it contains 96 samples (see mapping file)
plate1 <- DataAnalysis(plate_reader_file = "SamplePlateReader_Plate1.xlsx" , mapping_csv_file = "SampleMapping_Plate1.csv", exp_id = "Sample_Experiment", volume = 5, scale_factor = 14, standards_plate_reader_file = "SamplePlateReader_Plate2.csv", standards_mapping_csv_file = "SampleMapping_Plate2.csv")

## Plate 2 contains the standards
plate2 <- DataAnalysis(plate_reader_file = "SamplePlateReader_Plate2.csv" , mapping_csv_file = "SampleMapping_Plate2.csv", exp_id = "Sample_Experiment",  volume = 5, scale_factor = 14)


## ----StandardsOutput, echo=FALSE-----------------------------------------
plate1 <- DataAnalysis(plate_reader_file = "SamplePlateReader_Plate1.xlsx" , mapping_csv_file = "SampleMapping_Plate1.csv", exp_id = "Sample_Experiment", volume = 5, scale_factor = 14, standards_plate_reader_file = "SamplePlateReader_Plate2.csv", standards_mapping_csv_file = "SampleMapping_Plate2.csv")

## ----Standards, eval=T, fig.path='figure/', warning=FALSE, include=T, messages = F, results = 'hide'----
standards <- StandardAnalysis(standards_plate_reader_file = "SamplePlateReader_Plate2.csv", standards_mapping_csv_file = "SampleMapping_Plate2.csv", exp_id = "Sample_Experiment")

## ----CombineData, eval=T, warning=FALSE, include=T-----------------------
sample_experiment <- rbind(plate1$data, plate2$data)

## ----Save, eval=FALSE, message=FALSE, warning=FALSE, include=T-----------
#  write.csv(x = sample_experiment, file = 'sample_experiment_data.csv', row.names = F)

## ----PrintTable, echo=FALSE, results="asis"------------------------------
pander::pandoc.table(head(sample_experiment))

## ----16SPrep, eval=F, fig.path='figure/', warning=FALSE, include=T, messages = F----
#  sequencing_runs_16S <- sequencing_prep_16S(experiment_data_list = list(sample_experiment), n_barcode_plates = 2)

## ----MetagenomicsPrep, eval=F, fig.path='figure/', warning=FALSE, include=T, messages = F----
#  sequencing_runs_metagenomics <- sequencing_prep_metagenomics(experiment_data_list = list(sample_experiment), n_barcode_plates = 2)

