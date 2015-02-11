\name{BatchScale}
\alias{BatchScale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch Scaling of Taxonomic Data
}
\description{
This function will scale the taxonomic data from QIIME's summarize_taxa_through_plots.py function with given biomass data.
}
\usage{
BatchScale(biomass_file, taxonomy_directory, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{biomass_file}{
This file contains biomass information for your particular variables. Generally, this should be a .csv file with your variables in the first column and the biomass data (with "Biomass" as the heading) in the second column. See the SampleScaling folder on the github page for examples (www.github.com/econtijoch/Biomass-Workflow).
}
  \item{taxonomy_directory}{
  This should be the path to the directory in which the taxonomy files are located (this will generally just be the output of the QIIME script). For examples on what these may look like, please see the SampleScaling folder on the github page (www.github.com/econtijoch/Biomass-Workflow).
  }
  \item{OutputPrefix}{
  This OPTIONAL argument can be used to label your output files with a particular name. DEFAULT = "Scaled"
  }
  \item{XAxisLabel}{
  This OPTIONAL arguemnt can be used to force the labeling of the x-axis of your new plots. By default, this uses the information from the taxonomy tables from QIIME and the information from your biomass file, but they MUST agree exactly. By calling this argument, you override the default, but may end up with mis-labeled categories if not careful
  }
  \item{YAxisLabel}{
  This OPTIONAL argument can be used to change the y-axis label. DEFAULT = "ug DNA per mg Fecal Pellet"
  }
  \item{OutputDirectory}{
  This OPTIONAL argument can be used to change the directory of the output folder created for your data. DEFAULT = "." It is NOT necessary to include a trailing "/" for your file name (e.g. OutputDirectory = "output_files")
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a list with two items, as well as a folder within which you may find .csv files containing the scaled taxonomic tables.
%%  If it is a LIST, use
\item{master_taxonomy_scaled}{This is a data frame with all of the scaled data combined. There is a column to indicate the taxonomic level for the data. This can be useful if you want to look at the raw data in R for further analysis}
\item{figures}{This is a list (yes, a list within a list) containing all of the figures produced by the function (that is, one figure per taxonomic level, scaled).}

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

--- These examples can be run on the data included in the folder 'SampleScaling' found at the github page (www.github.com/econtijoch/Biomass-Workflow).

> BMSControl <- BatchScale(biomass_file = 'ControlBiomass.txt', taxonomy_directory = 'Control_Diet/', XAxisValues = c("-14", "-7", "0", "1", "2", "7", "13"), batchName = "Control", OutputDirectory = "Scaled_Control")
> BMSCipro <- BatchScale(biomass_file = 'ControlBiomass.txt', taxonomy_directory = 'Control_Diet/', XAxisValues = c("-14", "-7", "0", "1", "2", "7", "13"), batchName = "Control", OutputDirectory = "Scaled_Control")
> BMSHLLP <- BatchScale(biomass_file = 'HLLPBiomass.txt', taxonomy_directory = 'HLLP/', XAxisValues = c("-14", "-7", "0", "1", "2", "7", "13"), batchName = "HLLP", OutputDirectory = "Scaled_HLLP")
> HuCross <- BatchScale(biomass_file = 'HuCrossBiomass.txt', taxonomy_directory = 'HuCross/', batchName = "HuCross", OutputDirectory = "Scaled_HuCross")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
