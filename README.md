<!-- README.md is generated from README.Rmd. Please edit that file -->
BiomassWorkflow
===============

The goal of BiomassWorkflow is to streamline data analysis and removing
the potential for errors in the process. The package is most useful for
generating biomass data and comparing biomass between various groups.
There are also a couple of helper functions included and other scripts
that help with moving samples through the processing pipeline.

Installation
------------

The full installation of this package is a bit complicated because it
requires various dependencies in order to work fully.

First, you will need to install a few things in R in order to get this
package to work. Simply follow the following commands:

    # Install devtools, a package that helps install other packages from GitHub/Bitbucket
    install.packages("devtools")
    
    # Update to the development verison (0.4.0) of the biom R package that can accept hdf5 .biom files
    devtools::install_github('joey711/biom')
    
    # Source BiocLite and install rhdf5 - a package necessary for being able to read hdf5-formatted .biom tables
    source("https://bioconductor.org/biocLite.R")
    biocLite("rhdf5")
    biocLite("phyloseq")

    # Install this package ("BiomassWorkflow") from Bitbucket (or GitHub via devtools::install_github('econtijoch/Biomass-Workflow') )
    devtools::install_bitbucket("econtijoch/biomass-workflow")  


There is a very strong chance that this alone will not be enough. You
will also need to set up java to play nicely with R and this package. To
do that, you'll need to go to terminal and type:

    sudo R CMD javareconf
    sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

This will prompt you for your password in order to execute.

Then, back in R, you will need to re-install the rJava library:

    install.packages("rJava",type='source')

That should be all you need to do to get the package working. If for
some reason that is not enough, you may need to install the [latest
version of Java](https://www.java.com/en/download/manual.jsp), and then
go through the last few steps again.

Tutorial
--------

I am working on updating the documentation for the tutorial. I'll get
around to it soon.