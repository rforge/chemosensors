
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<meta content="R package chemosensors, machine olfaction, synthetic datasets, benchmarks, data generation tool" name="keywords" />

	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<h3>R package chemosensors</h3>
<p>The development of chemosensors package was originated in <a href="http://neurochem.sisbio.recerca.upc.edu/">NEUROChem project</a> with requirements of large-scale gas sensor array data to run simulations on artificial olfaction. This package introduces a software tool that allows for the design of synthetic experiments with so-called virtual gas sensor arrays.</p>

<p>The generator of sensor signals can be used in applications related to educational tools, neuromorphic simulations in machine olfaction, and test and benchmarking of signal processing methods.</p>

<p>The synthetic array of sensors allows for the generation of chemosensor data with a variety of characteristics: unlimited number of sensors, support of multicomponent gas mixtures and full parametric control of the noise in the system. 
</p>

<p>The released package makes use of the object-oriented programming paradigm (S4 classes), supports parallel computing and contains the datasets exploited in projects of the authors.</p>

<!-- end of project description -->

<h3>Installation</h3>
<p>Chemosensors package can be installed as a regular R package from the R-Forge repository. The command to type in R:</p>
<pre>
install.packages("chemosensors", dep=TRUE, repos="http://r-forge.r-project.org")
</pre>
<p>That will install the latest development version with all dependencies.</p>

<h3>Documetation</h3>
<p>Help pages in html format are available on the UPC server <a href="http://neurochem.sisbio.recerca.upc.edu/public/chemosensors/html/00Index.html">http://neurochem.sisbio.recerca.upc.edu/public/chemosensors/html/00Index.html</a>.</p> 

<h3>Examples</h3>
<p>You might prefer to start with demos of the package. To see the list of available demos type in R:</p>
<pre>
demo(package="chemosensors")
</pre>

<p>Basic commands to generate synthetic data from a virtual sensor array could be:</p>
<pre>
# concentration matrix of 3 gas classes: A, C and AC
conc <- matrix(0, 300, 3)
conc[1:100, 1] <- 0.05 # A
conc[101:200, 3] <- 1 # C
conc[201:300, 1] <- 0.05 # AC
conc[201:300, 3] <- 1 # AC

conc <- conc[sample(1:nrow(conc)  ), ]

# sensor array of 5 sensors with parametrized noise levels
sa <- SensorArray(num=1:5, csd=0.1, ssd=0.1, dsd=0.1)

# get information about the array
print(sa)
plot(sa)

# generate the data
sdata <- predict(sa, conc)

# plot the data
plot(sa, "prediction", conc=conc)
</pre>

<h3>Animation demo</h3>
<p>This animation presents a simulation of synthetic data with different noise parameters. The synthetic data (top of the graphics) is visually compared with the reference UNIMAN data (bottom of the graphics) by plotting PCA scores.</p>

<p>Objective of the simulation is to reproduce the reference dataset by playing with combinations of the parameters (barplot on the graphics). The virtual sensor array is composed of the same number of sensors (17) as the UNIMAN array. The concentration profile of 200 samples contains eight gas classes (legend of the graphics).</p> 

<p>Three noise parameters represent concentration noise (csd), sensor noise (ssd) and drift (dsd).</p>

<p><object width="550" height="360" classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000" codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,40,0"><param name="src" value="uniman-vsa-200.swf" /><embed width="1100" height="720" type="application/x-shockwave-flash" src="uniman-vsa-200.swf" /></object></p>

<h3>References</h3>

<p>The project summary page you can find on <a href="http://r-forge.r-project.org/projects/chemosensors/">development page</a> on the R-Forge website.</p>

<p><a href="http://neurochem.sisbio.recerca.upc.edu/?page_id=86">The UPC site for Neurochem project</a> is another source of additional information.</p>

<h3>Contacts</h3>
<p>Alexandre Perera<br>
Email: alexandre.perera [at] upc.edu</p>

<p>Andrey Ziyatdinov<br>
Email: andrey.ziyatdinov [at] upc.edu<p>

<p>
Address:<br>
Universitat Politècnica de Catalunya, dept. ESAII<br>
c/ Pau Gargallo 5, 08028 Barcelona, Spain<br>
Tel.: +34 93 407 07 73</p>

<h3>Acknowledgment</h3>
<p>This work was funded from the European Community's Seventh Framework Programme (FP7/2007-2013) under grant agreement no. 216916: Biologically inspired computation for chemical sensing (NEUROChem), the Ramon y Cajal program from the Spanish Ministerio de Educacion y Ciencia and TEC2010-20886-C02-02. CIBER-BBN is an initiative of the Spanish ISCIII.</p>
</body>
</html>
