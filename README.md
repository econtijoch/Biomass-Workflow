<h2>Biomass Data Analysis Helper Tools</h2>
I developed an R package that is helpful in streamlining data analysis and removing the potential for errors in the process. The package is most useful for generating biomass data and comparing biomass between various groups.
The package consists of four functions that help parse through the output files of the Promega plate reader for data, use a mapping file to help bridge what the information from the plate reader tells us about our specific samples samples, and then computes information regarding the DNA content, concentration, and biomass ratio for our samples.

The package can be downloaded from within R using the devtools function:

<pre>
devtools::install_github("econtijoch/Biomass-Workflow")
</pre>

Each of the functions has its own documentation that can further explain their usage. The github repository contains a sample mapping file that can be used as a basis for your own mapping files.
