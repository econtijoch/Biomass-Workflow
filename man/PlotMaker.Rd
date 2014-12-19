\name{PlotMaker}
\alias{PlotMaker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMaker(analyzed_data, exp_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{analyzed_data}{
%%     ~~Describe \code{analyzed_data} here~~
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
function (analyzed_data, exp_id) 
{
    plot <- ggplot(analyzed_data, aes(x = Condition, y = biomass_ratio, 
        fill = Condition)) + geom_boxplot() + guides(fill = FALSE) + 
        stat_summary(fun.y = mean, geom = "point", shape = 5, 
            size = 4) + ggtitle(exp_id) + ylab("Biomass (ug DNA per mg fecal pellet)") + 
        theme(plot.title = element_text(face = "bold")) + ylim(-0.05, 
        1.25)
    png(paste(exp_id, "Plot.png"))
    print(ggplot(analyzed_data, aes(x = Condition, y = biomass_ratio, 
        fill = Condition)) + geom_boxplot() + guides(fill = FALSE) + 
        stat_summary(fun.y = mean, geom = "point", shape = 5, 
            size = 4) + ggtitle(exp_id) + ylab("Biomass (ug DNA per mg fecal pellet)") + 
        theme(plot.title = element_text(face = "bold")) + ylim(-0.05, 
        1.25))
    dev.off()
    return(plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
