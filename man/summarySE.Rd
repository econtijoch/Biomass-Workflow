\name{summarySE}
\alias{summarySE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summarySE(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{measurevar}{
%%     ~~Describe \code{measurevar} here~~
}
  \item{groupvars}{
%%     ~~Describe \code{groupvars} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{conf.interval}{
%%     ~~Describe \code{conf.interval} here~~
}
  \item{.drop}{
%%     ~~Describe \code{.drop} here~~
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
function (data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, 
    conf.interval = 0.95, .drop = TRUE) 
{
    require(plyr)
    length2 <- function(x, na.rm = FALSE) {
        if (na.rm) 
            sum(!is.na(x))
        else length(x)
    }
    datac <- ddply(data, groupvars, .drop = .drop, .fun = function(xx, 
        col) {
        c(N = length2(xx[[col]], na.rm = na.rm), mean = mean(xx[[col]], 
            na.rm = na.rm), sd = sd(xx[[col]], na.rm = na.rm))
    }, measurevar)
    datac <- rename(datac, c(mean = measurevar))
    datac$se <- datac$sd/sqrt(datac$N)
    ciMult <- qt(conf.interval/2 + 0.5, datac$N - 1)
    datac$ci <- datac$se * ciMult
    return(datac)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
