\name{StandardAnalysis}
\alias{StandardAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Analysis
}
\description{
This function analyzes the standards from a fluorometer file given a mapping file
}
\usage{
StandardAnalysis(standards_plate_reader_csv_file, standards_mapping_csv_file, exp_id, BR_or_HS, std600)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
standard_table1 <- StandardAnalysis(standards_plate_reader_csv_file = "RawStandards.csv", standards_mapping_csv_file = "StandardsMapping.csv", exp_id = "TST", BR_or_HS = "BR", std600 = "y")

standard_table2 <- StandardAnalysis(standards_plate_reader_csv_file = "RawStandards.csv", standards_mapping_csv_file = "StandardsMapping.csv", exp_id = "TST", BR_or_HS = "HS", std600 = "ne")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Biomass }

