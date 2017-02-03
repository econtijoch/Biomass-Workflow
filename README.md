
<!-- README.md is generated from README.Rmd. Please edit that file -->
BiomassWorkflow
===============

The goal of BiomassWorkflow is to streamline data analysis and removing the potential for errors in the process. The package is most useful for generating biomass data and comparing biomass between various groups. There are also a couple of helper functions included and other scripts that help with moving samples through the processing pipeline.

Installation
------------

From within R, you can install BiomassWorkflow from scratch from bitbucket with:

``` r
install.packages("devtools")
devtools::install_bitbucket("econtijoch/Biomass-Workflow")
```
There is a very strong chance that this alone will not be enough. You will also need to set up java to play nicely with R and this package. To do that, you'll need to go to terminal and type

```sh
sudo R CMD javareconf
```
This will prompt you for your password in order to execute

Then, back in R, you will need to re-install the rJava library:

```r
install.packages("rJava",type='source')
```

You will also need to add a couple more packages that are not able to be pulled in directly from the original installation. Go through these commands and then restart your R session.

```r
source("https://bioconductor.org/biocLite.R")
biocLite("rhdf5")
devtools::install_github('joey711/biom')
```


Tutorial
--------

I am working on updating the documentation for the tutorial. I'll get around to it soon.
