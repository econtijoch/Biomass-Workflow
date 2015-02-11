<h2>Biomass Data Analysis Helper Tools</h2>
<p>An R package that is helpful in streamlining data analysis and removing the potential for errors in the process. The package is most useful for generating biomass data and comparing biomass between various groups.</p>
<p>The package consists of four functions that help parse through the output files of the Promega plate reader for data, use a mapping file to help bridge what the information from the plate reader tells us about our specific samples samples, and then computes information regarding the DNA content, concentration, and biomass ratio for our samples.</p>
<p>There is also a function to help scale taxonomic data with biomass data. Sample data for this function can be found in 'SampleScaling/', and the usage of the function is included in its documentation.</p>

<p>The package can be downloaded from within R using the devtools function:</p>

<pre>
devtools::install_github("econtijoch/Biomass-Workflow")
</pre>

If you need to install devtools first, you'll need to do the following first to download and update devtools to the most current version:

<pre>
	install.packages("devtools")
	devtools::install_github("hadley/devtools")
</pre>

<p>Each of the functions has its own documentation that can further explain their usage. Here in the github, I have posted a sample mapping file that can be used as a basis for your own mapping files (SampleMapping.csv).</p>

<h3> Known Issues </h3>
<ul>
	<li>The .csv file that the PlateReader produces does not code the data correctly. Specifically, it doesn't create a csv file that would correspond to a table with even numbers of columns in the rows. An easy, but annoying way around this is to open the .csv file in Excel, and simply save it (as it is, as a .csv file). Excel is able to read the data in correctly, and then when saving in Excel, it adds the correct formatting of the commas to create an even array of columns. Once you save the .csv file through excel, the data can be processed without issues with the functions of this package.</li>
</ul>