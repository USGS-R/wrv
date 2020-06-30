# wrv

[![Travis-CI Build Status](https://travis-ci.org/USGS-R/wrv.svg?branch=master)](https://travis-ci.org/USGS-R/wrv)
[![USGS Category](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)

## Overview

The [R](https://www.r-project.org/) package **wrv** is for processing the
groundwater-flow model of the Wood River Valley (WRV) aquifer system, south-central Idaho.
The groundwater-flow model is described in the associated
[model report](https://doi.org/10.3133/sir20165080 "USGS Scientific Investigations Report") and
[model archive](https://doi.org/10.5066/F7C827DT "USGS Data Release").
Included in the package is [MODFLOW-USG](https://water.usgs.gov/ogw/mfusg/ "MODFLOW-USG") version 1.3,
a U.S. Geological Survey (USGS) computer code that solves the groundwater-flow equation.

## Install

You can install **wrv** from [GitHub](https://github.com/USGS-R/wrv),
and its dependencies from [CRAN](https://cran.r-project.org/),
using the following commands:

```r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("USGS-R/wrv", dependencies = TRUE)
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

See [training material](https://jfisher-usgs.github.io/wrv-training/) for example applications using the **wrv** package.

## Contact

Please consider reporting bugs and asking questions on the [Issues page](https://github.com/USGS-R/wrv/issues).

## Disclaimer

This software has been approved for release by the U.S. Geological Survey
(USGS). Although the software has been subjected to rigorous review, the USGS
reserves the right to update the software as needed pursuant to further analysis
and review. No warranty, expressed or implied, is made by the USGS or the U.S.
Government as to the functionality of the software and related material nor
shall the fact of release constitute any such warranty. Furthermore, the
software is released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.
