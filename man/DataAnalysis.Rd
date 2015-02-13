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
DataAnalysis(plate_reader_csv_file, mapping_csv_file, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plate_reader_csv_file}{
The location of the raw .csv file from the Promega plate reader
}
  \item{mapping_csv_file}{
The location of your mapping file for your data. Experimental data should have 'Type' = 'Experiment'
}
  \item{standards_plate_reader_csv_file}{
OPTIONAL. This points to the location of your raw .csv file from the Promega plate reader that includes your standards. If not included, it will assume that your standards are on the same plate as your samples.
}
  \item{standards_mapping_csv_file}{
OPTIONAL. The location of your mapping file for your standards. Standards data should have 'Type' = 'Standard'. Use the 'SampleMass' column to indicate the total DNA in the well (for BR this should be 0, 50, 100, 200, 400, 600, 800, 1000; and for HS this should be 0, 5, 10, 20, 40, 60, 80 ,100). If not included, will assume that standards information is in the mapping file for your samples.
}
  \item{exp_id}{
A string for your experiment's ID to be used to create plots
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

data <- DataAnalysis(plate_reader_csv_file = "RawData.csv", mapping_csv_file = "DataMapping.csv", standards_plate_reader_csv_file = "RawStandards.csv", standards_mapping_csv_file  = "StandardsMapping.csv", exp_id = "TST")
data2 <- DataAnalysis(plate_reader_csv_file = "RawData.csv", mapping_csv_file = "DataMapping.csv", exp_id = "Test2")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Biomass }

