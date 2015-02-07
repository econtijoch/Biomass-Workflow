\name{ParseMappingFile}
\alias{ParseMappingFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parse Mapping File
}
\description{
A helper function that reads the data in a .csv mapping file and returns a data frame useful for combining with a parsed plate reader data frame.
}
\usage{
ParseMappingFile(mapping_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping_file}{
A .csv file containing information about the samples run on the plate reader. This file MUST have columns named "Well", containing the wells on the 96 well plate, "BarcodeID" with identifying barcodes for the samples, and "SampleMass" containing the mass of the input sample. Also, there must be a column named "Type" that contains values either "Experiment" or "Standard" to indicate if the well was used as a standard or as an experimental sample.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

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
ParseMappingFile('~/Path/To/mapping_file')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
