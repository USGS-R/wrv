# wrv

An [R](http://www.r-project.org/ "The R Project for Statistical Computing") package for processing the
groundwater-flow model of the Wood River Valley (WRV) aquifer system, south-central Idaho.
The groundwater-flow model is described in the associated
[model report](http://dx.doi.org/10.3133/sir20165080 "USGS Scientific Investigations Report") and
[model archive](http://dx.doi.org/10.5066/F7C827DT "USGS Data Release").
Included in the package is [MODFLOW-USG](http://water.usgs.gov/ogw/mfusg/ "MODFLOW-USG") version 1.2,
a U.S. Geological Survey (USGS) computer code that solves the groundwater-flow equation.
The set of standards used for coding the **wrv** package is documented in
[Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml "Google's R Style Guide").

## Install

If R is not already installed on your computer, download and install the latest binary distribution from the Comprehensive R Archive Network
([CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network")).
Next, open an R session and install user-contributed R packages from CRAN and the Geological Survey R Archive Network
([GRAN](http://owi.usgs.gov/R/gran.html "The Geological Survey R Archive Network")) using the following commands:

```r
repos <- c("http://owi.usgs.gov/R", getOption("repos"))
install.packages("wrv", repos = repos, dependencies = TRUE, type = "both")
```

## Run

Load the **wrv** package in the current R session:

```r
library(wrv)
```

Access package documentation:

```r
help(package = "wrv")
```

## Bugs

Please consider reporting bugs and asking questions on the [Issues page](https://github.com/USGS-R/wrv/issues).

## Disclaimer

This software is in the public domain because it contains materials that originally came from the USGS,
an agency of the United States Department of Interior.
For more information, see the
[official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy").

Although this software program has been used by the USGS, no warranty, expressed or implied,
is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)
