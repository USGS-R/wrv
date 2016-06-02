# wrv

An [R](http://www.r-project.org/ "The R Project for Statistical Computing") package for processing the numerical groundwater-flow model of the Wood River Valley (WRV) aquifer system, south-central Idaho.
The set of standards used for coding **wrv** is documented in [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml "Google's R Style Guide").
Included in the package is [MODFLOW-USG](http://water.usgs.gov/ogw/mfusg/ "MODFLOW-USG") version 1.2, a U.S. Geological Survey groundwater-flow model.

## Install

If R is not already installed on your computer, download and install the latest binary distribution from the Comprehensive R Archive Network ([CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network")).
Open an R session and install user-contributed R packages using the following commands:

```r
repos <- c("http://owi.usgs.gov/R", getOption("repos"))
update.packages(ask = FALSE, repos = repos)
install.packages("wrv", repos = repos, dependencies = TRUE, type = "both")
```

## Run

Load **wrv** in the current R session:

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

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior.
For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy").

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied,
is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)
