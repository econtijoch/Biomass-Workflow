\name{StandardAnalysis}
\alias{StandardAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StandardAnalysis(standards_plate_reader_csv_file, standards_mapping_csv_file, exp_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{standards_plate_reader_csv_file}{
%%     ~~Describe \code{standards_plate_reader_csv_file} here~~
}
  \item{standards_mapping_csv_file}{
%%     ~~Describe \code{standards_mapping_csv_file} here~~
}
  \item{exp_id}{
%%     ~~Describe \code{exp_id} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (standards_plate_reader_csv_file, standards_mapping_csv_file, 
    exp_id) 
{
    rawdata <- na.omit(read.csv(file = standards_plate_reader_csv_file, 
        sep = ",", header = TRUE, skip = 9, nrows = 96, skipNul = FALSE, 
        colClasses = c("NULL", "character", rep("numeric", 10))))
    mapping <- na.omit(read.csv(file = standards_mapping_csv_file, 
        sep = ",", fill = TRUE, header = TRUE, na.strings = "", 
        colClasses = c(rep("character", 8), "numeric")))
    data <- merge(rawdata, mapping, by = "Well")
    rownames(data) <- data$BarcodeID
    data$fluor_av <- apply(data[2:11], 1, mean)
    standard_table <- split(data, data$Type)$Standard
    standard_table <- standard_table[order(standard_table$BarcodeID), 
        ]
    exp_data <- split(data, data$Type)$Experiment
    s_y <- c(0, 50, 100, 200, 400, 600, 800, 1000)
    s_x <- standard_table$fluor_av
    standards <- data.frame(s_x, s_y)
    colnames(standards) <- c("x", "y")
    standard_curve <- lm(standards$y ~ standards$x)
    scale_x <- standard_curve$coefficients[2]
    intercept <- standard_curve$coefficients[1]
    rsquared <- summary(standard_curve)$r.squared
    name <- paste(exp_id, "Standard Curve.png")
    png(name)
    plot(standards, xlab = "Measurement", ylab = "Standard", 
        main = "Standard Curve")
    abline(standard_curve)
    text(1, 950, paste("Y =", round(scale_x, 5), "* X ", round(intercept, 
        5), "\nR^2 = ", round(rsquared, 5)), pos = 4)
    dev.off()
    return(standard_table)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
