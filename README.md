<h2>Biomass Data Analysis Helper Tools</h2>
<p>An R package that is helpful in streamlining data analysis and removing the potential for errors in the process. The package is most useful for generating biomass data and comparing biomass between various groups. There are also a couple of helper functions included and other scripts that help with moving samples through the processing pipeline.</p>

<p>The package can be downloaded from within R using the devtools function:</p>

<pre>
devtools::install_github("econtijoch/Biomass-Workflow")
</pre>

If you need to install devtools first, you'll need to do the following first to download and update devtools to the most current version:

<pre>
	install.packages("devtools")
	devtools::install_github("hadley/devtools")
</pre>

<p>Each of the functions has its own documentation that can further explain their usage (**working on updating this**). Here in the github, I have posted a sample mapping file that can be used as a basis for your own mapping files (SampleMapping.csv).</p>

<h3> Known Issues </h3>
<ul>
	<li>When reading in Excel files, only the first sheet will be read into the input. Plan accordingly and save each sheet as its own file.</li>
</ul>