# wrv

[![Travis-CI Build Status](https://travis-ci.org/USGS-R/wrv.svg?branch=master)](https://travis-ci.org/USGS-R/wrv)

## Overview

The [R](http://www.r-project.org/) package **wrv** is for processing the
groundwater-flow model of the Wood River Valley (WRV) aquifer system, south-central Idaho.
The groundwater-flow model is described in the associated
[model report](http://dx.doi.org/10.3133/sir20165080 "USGS Scientific Investigations Report") and
[model archive](http://dx.doi.org/10.5066/F7C827DT "USGS Data Release").
Included in the package is [MODFLOW-USG](http://water.usgs.gov/ogw/mfusg/ "MODFLOW-USG") version 1.3,
a U.S. Geological Survey (USGS) computer code that solves the groundwater-flow equation.

## Install

You can install the stable version of **wrv** from [GRAN](http://owi.usgs.gov/R/gran.html),
and its dependencies from [CRAN](http://cran.r-project.org/), using the following commands:

```r
repos <- c("http://owi.usgs.gov/R", getOption("repos"))
install.packages("wrv", repos = repos, dependencies = TRUE)
```

Or use **devtools** to install the development version from GitHub.

```r
devtools::install_github("USGS-R/wrv")
```

## Run

Load **wrv** in the current R session

```r
library(wrv)
```

Access package documentation

```r
help(package = "wrv")
```

## Bugs

Please consider reporting bugs and asking questions on the [Issues page](https://github.com/USGS-R/wrv/issues).

## Disclaimer

This software is in the public domain because it contains materials that originally came from the USGS,
an agency of the United States Department of Interior.
For more information, see the
[official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html "official USGS copyright policy").

Although this software program has been used by the USGS, no warranty, expressed or implied,
is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)
