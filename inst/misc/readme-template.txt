@{paste(strwrap(format(citation("wrv"), style="textVersion"), width=79), collapse="\n ")}

   05/25/2016 Model archive created.
   06/16/2016 Model archive update based on Watt, M.K. review of model archive.

--------------------------------------------------------------------------------

    SIR2016-5080/
        Description:
        -----------
        The underlying directories contain all of the input and output files for
        the simulation described in the report, processing scripts and datasets
        are bundled together in an R-package wrv, included in the package is the
        MODFLOW-USG (v 1.3.00) source code.

        Descriptions of the data in each subdirectory are given to facilitate
        understanding of this data release. File descriptions are provided for
        select files to provide additional information that may be of use for
        understanding this data release.

        Support is provided for correcting errors in the data release and
        clarification of the modeling conducted by the U.S. Geological Survey.
        Users are encouraged to review the complete model documentation report
        (http://dx.doi.org/10.3133/sir20165080) to understand the purpose,
        construction, and limitations of this model.

        Reconstructing the data release from the online data release:
        -------------------------------------------------------------
        This data release is available from:

            http://dx.doi.org/10.5066/F7C827DT

        The model, along with pre- and post-processing tools, will run
        successfully only if the original directory structure is correctly
        restored. The data release is broken into several pieces to reduce the
        likelyhood of download timeouts. Small files (readme.txt and
        modelgeoref.txt) are available as text files. All other files
        are compressed into ZIP files at the subdirectory level. Decompress
        the ZIP files in the highest-level directory.

        File and folder structure
        -------------------------
        SIR2016-5080
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
        |   +-- wrv_@{packageDescription('wrv', fields = 'Version')}.zip (Windows binary installer for the R-package wrv)
        +-- georef (polygon shapefile showing the maxiumum model extent,
        |           active and inactive model domains.)
        |   +-- sir2016_5080.dbf
        |   +-- sir2016_5080.prj
        |   +-- sir2016_5080.shp
        |   +-- sir2016_5080.shx
        +-- model (model input files)
        |   +-- model1
        |       +-- hk1.ref (hydraulic conductivity in model layer 1)
        |       +-- hk2.ref (hydraulic conductivity in model layer 2)
        |       +-- hk3.ref (hydraulic conductivity in model layer 3)
        |       +-- ss1.ref (storage coefficient in model layer 1)
        |       +-- ss2.ref (storage coefficient in model layer 2)
        |       +-- ss3.ref (storage coefficient in model layer 3)
        |       +-- usgs.model.reference (data to register the model in
        |                                 space and time)
        |       +-- $(id).ba6 (MODFLOW basic package)
        |       +-- $(id).dis (MODFLOW discretization data)
        |       +-- $(id).drn (MODFLOW drain package)
        |       +-- $(id).lpf (MODFLOW layer-property flow package)
        |       +-- $(id).nam (MODFLOW names of model input and output files)
        |       +-- $(id).oc  (MODFLOW output control options)
        |       +-- $(id).riv (MODFLOW river package)
        |       +-- $(id).sms (MODFLOW sparse-matrix solver package)
        |       +-- $(id).wel (MODFLOW well package)
        +-- output (model output files)
        |   +-- output.model1
        |       +-- $(id).bud (MODFLOW budget data)
        |       +-- $(id).hds (MODFLOW hydraulic head data)
        |       +-- $(id).lst (MODFLOW run listing)
        +-- source (source code used in this study)
        |   +-- wrv_@{packageDescription('wrv', fields = 'Version')}.tar.gz (R-package wrv source code)
        +-- modelgeoref.txt (model geo-reference information)
        +-- readme.txt

        File Formats
        ------------
        All digital data are stored using open file formats. "An open format is
        one that is platform independent, machine readable, and made available
        to the public without restrictions that would impede the re-use of that
        information" (Open Government Directive, M10-06).

        |Extension |Type   |Description                                |
        |----------|-------|-------------------------------------------|
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

        System requirements
        -------------------
        MODFLOW-USG 1.3 (2015-12-01)
        PEST 13.0 (2013-05-01)

        @{paste(capture.output(sessionInfo()), collapse="\n        ")}

        Running the model:
        ------------------
        On a @{paste(Sys.info()[c("sysname", "release")], collapse = " ")} OS (or similar build), open a command-prompt
        window in the ./model/model1 folder. Run the groundwater-flow model by
        initiating MODFLOW-USG from the command prompt using the following
        command:

            ../../bin/mfusg.exe "$(id).nam"

        Be forewarned that this command will overwrite any existing model output
        files.

        Instructions for processing (optional)
        --------------------------------------
        The set of instructions for processing the groundwater flow model can be
        run on multiple platforms (Windows, OS X, Linux). All processing
        programs are classified as Free and Open-Source Software (FOSS). With
        the exception of "model calibration", processing instructions are
        implemented in R, a programming language and computing environment.
        In R, the primary mechanism for sharing with others is the package.
        A package bundles together computer code, data, and documentation using
        a common format. While R comes with a set of default packages, model
        processing requires that many other packages be added to extend the
        capabilities of R. Because packages get updated all the time, it may be
        necessary to recreate an identical R environment where all your packages
        are consistent with package versions documented in this README file. To
        recreate the R environment as it existed when this model archive was
        built, first download and install @{R.version.string} from a
        snapshot of the Comprehensive R Archive Network (CRAN),
        https://mran.revolutionanalytics.com/snapshot/@{Sys.Date()}/.

        Open an R session and install user-contributed R packages from the CRAN
        snapshot by typing the following commands:

            repo <- "https://mran.revolutionanalytics.com/snapshot/@{Sys.Date()}/"
            pkgs <- c($(pkgs))
            install.packages(pkgs, repos = repo)

        Install the R-package wrv from a distribution file contained within
        the data release. Specify the absolute path to the model archive folder
        below (change "path" as needed).

            path <- "<path/to/archive>"  # construct using forward slash separators
            if (.Platform$OS.type == "windows") {
              file <- file.path(path, "bin/wrv_@{packageDescription('wrv', fields = 'Version')}.zip")
            } else {
              file <- file.path(path, "source/wrv_@{packageDescription('wrv', fields = 'Version')}.tar.gz")
            }
            install.packages(file, repos = NULL)

        Load the wrv package into the current R session:

            library(wrv)

        Access the wrv package documentation:

            help(package = "wrv")

        Open appendix A and follow the step-by-step instructions.

            vignette("package-introduction", package = "wrv")

        Web-based data storage
        ----------------------
        Source code and datasets are stored in the wrv-package repository; its
        contents are available on GitHub from:

            https://github.com/USGS-R/wrv

        GitHub is a web-based distributed revision control system. The only
        dataset not included in the wrv-package repository is the pre-processed
        topographic base map data for the study area. These data are part of the
        National Elevation Dataset and accessible through the National Map
        Viewer from:

            http://viewer.nationalmap.gov/viewer/
