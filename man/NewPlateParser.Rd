\name{NewPlateParser}
\alias{NewPlateParser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parse Plate Reader File (New)
}
\description{
This is a helper function that will parse a .csv file from the plate reader and return only the fluorescence data in a data frame, and the number of fluorescence reads that were acquired.
}
\usage{
NewPlateParser(plate_reader_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plate_reader_file}{
The path to your raw .csv file from the plate reader. ** NOTE: You may need to open the file in excel first before using it here, I've encountered an issue where it seems as though the raw .csv file is not well formatted from the plate reader, but opening it up in excel and saving it as a .csv file ameliorates this **
}
  \item{num_reads}{
A number indicating how many reads the fluorometer took. This will typically be 1 unless you took multiple measurements per plate.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
NewPlateParser('~/Path/To/plate_reader_file.csv', num_reads = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
