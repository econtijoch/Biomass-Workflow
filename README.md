
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
There is a strong chance that you will also need to set up java to play nicely with R and this package. To do that, you'll need to go to terminal and type

```sh
sudo R CMD javareconf
```
This will prompt you for your password.

Then, back in R, you will need to re-install the rJava library:

```r
install.packages("rJava",type='source')
```

Tutorial
--------

I am working on updating the documentation for the tutorial. I'll get around to it soon.
