## ----setup, include=FALSE------------------------------------------------
t0 <- Sys.time()
try(knitr::opts_chunk$set(tidy=FALSE, comment="#", fig.align="center"), silent=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  repos <- c("http://owi.usgs.gov/R", getOption("repos"))
#  update.packages(ask = FALSE, repos = repos)
#  install.packages("wrv", repos = repos, dependencies = TRUE)

## ----warning=FALSE, message=FALSE, results="hide"------------------------
library("wrv")

## ----eval=FALSE----------------------------------------------------------
#  help(package = "wrv")

## ----eval=FALSE----------------------------------------------------------
#  path <- file.path(getwd(), "SIR2016-5080")
#  dir.create(path, recursive = TRUE)
#  setwd(path)

## ----table_io, echo=FALSE, results="asis"--------------------------------
x <- c(".adf", "binary", "ArcGRID format, compressed in a ZIP file; raster graphic",
       ".tif", "binary", "Geo-referenced tagged Image File Format; raster graphic",
       ".shp", "binary", "Shapefile, compressed in a ZIP file; spatial points, poly-lines, and polygons",
       ".csv", "text",   "Comma-Separated Values; data table",
       ".kml", "text",   "Keyhole Markup Language; spatial polygons",
       ".ref", "text",   "Data reference file",
       ".rda", "binary", "R datasets",
       ".nam", "text",   "MODFLOW Name File",
       ".ba6", "text",   "MODFLOW Basic Package File",
       ".dis", "text",   "MODFLOW Structured Discretization File",
       ".sms", "text",   "MODFLOW Sparse Matrix Solver Package",
       ".oc",  "text",   "MODLFOW Output Control Option",
       ".lpf", "text",   "MODFLOW Layer-Property Flow Package",
       ".drn", "text",   "MODFLOW Drain Package",
       ".riv", "text",   "MODFLOW River Package",
       ".wel", "text",   "MODFLOW Well Package",
       ".exe", "binary", "MODFLOW compiled executable",
       ".bat", "text",   "Script file containing commands to execute",
       ".lst", "text",   "MODFLOW List File",
       ".hds", "binary", "MODFLOW Head File",
       ".bud", "binary", "MODFLOW Budget File",
       ".ptf", "text",   "PEST Template File")
d <- as.data.frame(matrix(x, ncol=3, byrow=TRUE), stringsAsFactors=FALSE)
d <- d[order(d[, 1]), ]
columns <- c("Extension", "Type", "Description")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
tbl <- xtable::xtable(d, label="table_io")
xtable::caption(tbl) <- "Input/output file formats."
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x},
      sanitize.text.function=identity, size="\\small")

## ----table_files, echo=FALSE, results="asis"-----------------------------
x <- c("mfusg.exe", "MODFLOW-USG executable",
       "RunModflow.bat", "Command to run the groundwater-flow model",
       "hk1.ref, hk2.ref, hk3.ref", "Hydraulic conductivity distribution in model layers 1, 2, and 3",
       "ss1.ref, ss2.ref, ss3.ref", "Storage coefficient distribution in model layers 1, 2, and 3",
       "model.rda", "Multiple datasets describing the model grid, stress periods, and so forth",
       "UpdateBudget.bat", "Command to update the water budget, requires access to \\R{}",
       "eff.csv", "Irrigation efficiencies",
       "trib.csv", "Flow conditions in the major tributary canyons",
       "seep.csv", "Canal seepage as a fraction of diversion",
       "qa-incidental.csv", "Quality assurance for incidental groundwater recharge on irrigated lands",
       "qa-natural.csv", "Quality assurance for natural groundwater recharge and discharge on non-irrigated lands",
       "qa-pumping.csv", "Quality assurance for groundwater diversions",
       "qa-well-config.csv", "Quality assurance for well configurations")

d <- as.data.frame(matrix(x, ncol=2, byrow=TRUE), stringsAsFactors=FALSE)
d <- d[order(d[, 1]), ]
columns <- c("Name", "Description")
colnames(d) <- sprintf("\\textbf{\\shortstack{%s}}", columns)
tbl <- xtable::xtable(d, label="table_files")
xtable::caption(tbl) <- "Files requiring additional clarification."
print(tbl, include.rownames=FALSE, caption.placement="top", booktabs=TRUE,
      sanitize.colnames.function=function(x){x},
      sanitize.text.function=identity, size="\\small")

## ----eval=FALSE----------------------------------------------------------
#  vignette("sir20165080_AppendixC")  # open appendix C
#  file <- system.file("doc", "sir20165080_AppendixC.R", package = "wrv")
#  source(file, echo = TRUE)  # or open file in a text editor and copy/paste into R console

## ----eval=FALSE----------------------------------------------------------
#  vignette("sir20165080_AppendixD")  # open appendix D
#  file <- system.file("doc", "sir20165080_AppendixD.R", package = "wrv")
#  source(file, echo = TRUE)  # or open file in a text editor and copy/paste into R console

## ----include=FALSE-------------------------------------------------------
v <- "Procedures used to create the \\textbf{wrv}-package datasets and process the uncalibrated groundwater-flow model."
v <- c(paste("Diagram showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----eval=FALSE----------------------------------------------------------
#  help("UpdateWaterBudget")  # open help documentation for function call
#  UpdateWaterBudget("model/model1", "wrv_mfusg", qa.tables = "english")

## ----include=FALSE-------------------------------------------------------
v <- "Procedures used in a single PEST run."
v <- c(paste("Diagram showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----include=FALSE-------------------------------------------------------
v <- "Procedures used when updating the water budget."
v <- c(paste("Diagram showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

## ----include=FALSE-------------------------------------------------------
v <- "Procedures used in the model-calibration process."
v <- c(paste("Diagram showing", paste0(tolower(substr(v, 1, 1)), substr(v, 2, nchar(v)))), v)

