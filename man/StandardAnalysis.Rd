\name{StandardAnalysis}
\alias{StandardAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Analysis
}
\description{
This function analyzes the standards from a fluorometer file given a mapping file. It will create a plot of your standard curve, and print to the console information regarding the line of best fit.
}
\usage{
StandardAnalysis(standards_plate_reader_csv_file, standards_mapping_csv_file, exp_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{standards_plate_reader_csv_file}{
The location of your raw .csv file from the Promega plate reader that includes your standards.
}
  \item{standards_mapping_csv_file}{
The location of your mapping file for your standards. Standards data should have 'Type' = 'Standard'. Use the 'SampleMass' column to indicate the total DNA in the well (for BR this should be 0, 50, 100, 200, 400, 600, 800, 1000; and for HS this should be 0, 5, 10, 20, 40, 60, 80 ,100)
}
  \item{exp_id}{
A string for your experiment's ID to be used to create plots
}
}
\details{
Produces a standard table and a plot giving you an opportunity to view the standards.
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
standard_table <- StandardAnalysis(standards_plate_reader_csv_file = "RawStandards.csv", standards_mapping_csv_file = "StandardsMapping.csv", exp_id = "TST")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Biomass }

