\name{DataAnalysis}
\alias{DataAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Analysis
}
\description{
This function will analyze biomass data given mapping files and raw .csv files from the Promega Fluorometer. This helps in streamlining data analysis for biomass workflows.
}
\usage{
DataAnalysis(plate_reader_csv_file, mapping_csv_file, standards_plate_reader_csv_file, standards_mapping_csv_file, exp_id, BR_or_HS, std600)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plate_reader_csv_file}{
The location of the raw .csv file from the Promega plate reader. Must have 10 reads per sample.
}
  \item{mapping_csv_file}{
The location of your mapping file for your data. Experimental data should have 'Type' = 'Experiment'
}
  \item{standards_plate_reader_csv_file}{
The location of your raw .csv file from the Promega plate reader that includes your standards. Must have 10 reads per sample.
}
  \item{standards_mapping_csv_file}{
The location of your mapping file for your standards. Standards data should have 'Type' = 'Standard'
}
  \item{exp_id}{
A string for your experiment's ID to be used to create plots
}
  \item{BR_or_HS}{
  A string of "HS" or "BR" to indicate which standards and QuBit dye used.
  }
  \item{std600}{
  A string "y" or "n" to indicate if the BR 600 ng standard was included or not.
  }
}
\details{
This function will analyze data and produce a table of your analysis as well as a standard curve.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Eduardo Contijoch
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data <- DataAnalysis(plate_reader_csv_file = "RawData.csv", mapping_csv_file = "DataMapping.csv", standards_plate_reader_csv_file = "RawStandards.csv", standards_mapping_csv_file  = "StandardsMapping.csv", exp_id = "TST", BR_or_HS = "BR", std600 = "n")

data2 <- DataAnalysis(plate_reader_csv_file = "RawData.csv", mapping_csv_file = "DataMapping.csv", standards_plate_reader_csv_file = "RawStandards.csv", standards_mapping_csv_file  = "StandardsMapping.csv", exp_id = "TST2", BR_or_HS = "HS", std600 = "y")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Biomass }

