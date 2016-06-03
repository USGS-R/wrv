# SIR2016-5080 Model Archive

This is an archive for the groundwater-flow model of the Wood River Valley aquifer system, south-central Idaho.
The purpose of this model archive is to:
(1) support and validate the results in the published report,
(2) assure that a working version of the model is available for future scientific use, and
(3) assure that the data are available to the public.

## Published Report

@{format(citation("wrv"), style="textVersion")}

## File and Folder Structure

    .
    +-- ancillary
    |   +-- calibrated
    |       +-- animation (depth to water table)
    |           +-- css
    |               +-- *.css
    |           +-- images
    |               +-- *.png
    |           +-- js
    |               +-- *.js
    |           +-- water-table.html
    |       +-- model (raster data within the model extent)
    |           +-- csv
    |               +-- *.csv
    |           +-- kml
    |               +-- rasters.kmz
    |           +-- png
    |               +-- *.png
    |           +-- RDA
    |               +-- rasters.rda
    |           +-- tif
    |               +-- *.tif
    |   +-- modelgrid (shapefile showing active and inactive model cells)
    |       +-- grid.dbf
    |       +-- grid.prj
    |       +-- grid.shp
    |       +-- grid.shx
    |   +-- pest
    |       +-- $(id).pst (PEST input control file)
    |   +-- uncalibrated
    |       +-- animation (depth to water table)
    |           +-- css
    |               +-- *.css
    |           +-- images
    |               +-- *.png
    |           +-- js
    |               +-- *.js
    |           +-- water-table.html
    |       +-- data (raster data within the aquifer system extent)
    |           +-- csv
    |               +-- *.csv
    |           +-- kml
    |               +-- rasters.kmz
    |           +-- png
    |               +-- *.png
    |           +-- rda
    |               +-- rasters.rda
    |           +-- tif
    |               +-- *.tif
    |       +-- model (raster data within the model extent)
    |           +-- csv
    |               +-- *.csv
    |           +-- kml
    |               +-- rasters.kmz
    |           +-- png
    |               +-- *.png
    |           +-- rda
    |               +-- rasters.rda
    |           +-- tif
    |               +-- *.tif
    +-- bin (compiled code used in study)
    |   +-- mfusg.exe (MODFLOW-USG 64-bit executable)
    |   +-- wrv_@{packageDescription('wrv', fields = 'Version')}.zip (Windows binary installer for the R-package **wrv**)
    +-- georef (polygon shapefile showing the maxiumum extent of the study area)
    |   +-- domain.dbf
    |   +-- domain.prj
    |   +-- domain.shp
    |   +-- domain.shx
    +-- model (model input files)
    |   +-- model1
    |       +-- hk1.ref (hydraulic conductivity in model layer 1)
    |       +-- hk2.ref (hydraulic conductivity in model layer 2)
    |       +-- hk3.ref (hydraulic conductivity in model layer 3)
    |       +-- ss1.ref (storage coefficient in model layer 1)
    |       +-- ss2.ref (storage coefficient in model layer 2)
    |       +-- ss3.ref (storage coefficient in model layer 3)
    |       +-- usgs.model.reference.txt (data to register the model in space and time)
    |       +-- $(id).ba6 (MODFLOW basic package)
    |       +-- $(id).dis (MODFLOW discretization data)
    |       +-- $(id).drn (MODFLOW drain package)
    |       +-- $(id).lpf (MODFLOW layer-property flow package)
    |       +-- $(id).nam (MODFLOW names of model input and output files)
    |       +-- $(id).oc  (MODFLOW output control options)
    |       +-- $(id).riv (MODFLOW river package)
    |       +-- $(id).sms (MODFLOW sparse-matrix solver package)
    |       +-- $(id).wel (MODFLOW well package)
    +-- model (model output files)
    |   +-- output.model1
    |       +-- $(id).bud (MODFLOW budget data)
    |       +-- $(id).hds (MODFLOW hydraulic head data)
    |       +-- $(id).lst (MODFLOW run listing)
    +-- source (source code used in this study)
    |   +-- wrv_@{packageDescription('wrv', fields = 'Version')}.tar.gz (R-package **wrv** source code)
    +-- modelgeoref.md (model geo-reference information)
    +-- readme.txt

## File Formats

All digital data are stored using open file formats.
"An open format is one that is platform independent, machine readable, and
made available to the public without restrictions that would impede the re-use of that information"
(Open Government Directive, [M10-06](http://www.whitehouse.gov/open/documents/open-government-directive)).

|Extension |Type   |Description                                |
|:---------|:------|:------------------------------------------|
|          |text   |Plain text                                 |
|.ba6      |text   |MODFLOW                                    |
|.bat      |text   |Script file containing commands to execute |
|.bud      |binary |MODFLOW                                    |
|.css      |text   |Cascading Style Sheets                     |
|.csv      |text   |Comma-Separated Values                     |
|.dis      |text   |MODFLOW                                    |
|.dbf      |binary |Shapefile                                  |
|.drn      |text   |MODFLOW                                    |
|.exe      |binary |Windows executable file                    |
|.hds      |binary |MODFLOW                                    |
|.html     |text   |Hyper Text Markup Language                 |
|.js       |text   |Javascript Language                        |
|.kml      |text   |Keyhole Markup Language                    |
|.lpf      |text   |MODFLOW                                    |
|.lst      |text   |MODFLOW                                    |
|.md       |text   |Markdown                                   |
|.nam      |text   |MODFLOW                                    |
|.oc       |text   |MODFLOW                                    |
|.png      |binary |Portable Network Graphics                  |
|.prj      |text   |Shapefile                                  |
|.pst      |text   |PEST input control file                    |
|.rda      |binary |Serialized versions of R objects           |
|.ref      |text   |Reference data                             |
|.riv      |text   |MODFLOW                                    |
|.shp      |binary |Shapefile                                  |
|.shx      |binary |Shapefile                                  |
|.sms      |text   |MODFLOW                                    |
|.tar.gz   |binary |R-package source code and datasets         |
|.tif      |binary |Geo-referenced Tagged Image File Format    |
|.txt      |text   |Plain text                                 |
|.wel      |text   |MODFLOW                                    |
|.zip      |binary |Compiled R package for Windows             |

## Software and Platform Versions

```
MODFLOW-USG 1.2.00 (2014-03-21)
PEST 13.0 (2013-05-01)
@{paste(capture.output(sessionInfo()), collapse = "\n")}
```

## Instructions for Running Model

On a @{paste(Sys.info()[c("sysname", "release")], collapse = " ")} OS (or similar build), open a command-prompt window in the `./model/model1` folder.
Run the groundwater-flow model by initiating [MODFLOW-USG](http://water.usgs.gov/ogw/mfusg/) from the command prompt using the following command:

```
../../bin/mfusg.exe "$(id).nam"
```

Be forewarned that this command will overwrite any existing model output files.

## Instructions for Processing (optional)

The set of instructions for processing the groundwater flow model can be run on multiple platforms (Windows, OS X, Linux).
All processing programs are classified as Free and Open-Source Software ([FOSS](http://en.wikipedia.org/wiki/Free_and_open-source_software)).
With the exception of "model calibration", processing instructions are implemented in [R](http://www.r-project.org/), a programming language and computing environment.
In R, the primary mechanism for sharing with others is the *package*.
A package bundles together computer code, data, and documentation using a common format.
While R comes with a set of default packages, model processing requires that many other packages be added to extend the capabilities of R.
Because packages get updated all the time, it may be necessary to recreate an identical R environment where all your packages are consistent with package versions documented in this README file.
To recreate the R environment as it existed when this model archive was built, first download and install @{R.version.string} from a
[snapshot](https://mran.revolutionanalytics.com/snapshot/@{packageDescription("wrv", fields = "Date")}/)
of the Comprehensive R Archive Network (CRAN).

Open an R session and install user-contributed R packages from the CRAN snapshot by typing the following commands:

```r
repo <- "https://mran.revolutionanalytics.com/snapshot/@{packageDescription('wrv', fields = 'Date')}/"
pkgs <- c($(pkgs))
install.packages(pkgs, repos = repo, type = "both")
```

Install the R-package **wrv** from a distribution file contained within the model archive.
Specify the absolute path to the model archive folder below (change `path` as needed).

```r
path <- "<path/to/archive>"  # construct path using forward slash separators
if (.Platform$OS.type == "windows") {
  file <- file.path(path, "bin/wrv_@{packageDescription('wrv', fields = 'Version')}.zip")
} else {
  file <- file.path(path, "source/wrv_@{packageDescription('wrv', fields = 'Version')}.tar.gz")
}
install.packages(file, repos = NULL)
```

Load the **wrv** package into the current R session:

```r
library(wrv)
```

Access the **wrv** package documentation:

```r
help(package = "wrv")
```

Open appendix A and follow the step-by-step instructions.

```r
vignette("package-introduction", package = "wrv")
```


## Web-based Data Storage

Source code and datasets are stored in the **wrv**-package repository;
its contents are available on [GitHub](https://github.com/USGS-R/wrv), a web-based distributed revision control system.
The only dataset not included in the **wrv**-package repository is the pre-processed topographic base map data for the study area.
These data are part of the National Elevation Dataset ([NED](http://nationalmap.gov/elevation.html)) and
accessible through the [National Map Viewer](http://viewer.nationalmap.gov/viewer/).
