# wrv 1.1.1.9000

- In `GetWellConfig` function, remove wells located outside the model grid.

# wrv 1.1.1

- Change graphic dimensions for figures in vignettes.
  Dimensions are now compatible with plotting routines in the latest version of the **inlmisc** package (v0.2.3).

- Set `integer64 = "allow.loss"` (default < rgdal 1.2) in calls to `rgdal::readOGR` function.

- Set `scale.loc = "bottomright"` in calls to `inlmisc::PlotMap` function.

- Update URL links to HTTP Secure

# wrv 1.1.0

- Add suggested package **leaflet**, allows the creation of interactive maps using the 'Leaflet' JavaScript library and the **htmlwidgets** package.

- Update MODFLOW-USG version from 1.2 to 1.3, see [release notes](https://water.usgs.gov/ogw/mfusg/release.txt) for details.

- Change NEWS file to markdown format.

- Remove **RCurl** from suggested packages.

- Recreate package datasets using Appendix C code, no significant changes were identified.

- Pass datasets as arguments in the `UpdateWaterBudget` and `RunWaterBudget` functions;
  these datasets were previously called from within the function.

- Add code block identifiers in vignettes.

# wrv 1.0.3

- Remove `DownloadFile` function and edit Appendix C to reflect this change.

- Add Makefile for automating package-development tasks.

- Change to in-source documentation using **roxygen2** package.

- Change file name from sir2016_5080_usgsdatarelease.xml to sir2016-5080_usgsdatarelease.xml, a Water Node request.

# wrv 1.0.2

- Fix NOTES associated with running `R CMD check`.

- Add published Scientific Investigations Report 2016-5080 to ./inst/doc folder.

# wrv 1.0.1

- Place vignette source, R-code, and PDF (optimized for viewing) files in the ./inst/doc folder.

# wrv 1.0.0

- Respond to Model Archive data release review comments.

- Add report number to documentation.

# wrv 0.8.1

- Move remote repository location from https://github.com/jfisher-usgs/wrv to https://github.com/USGS-R/wrv

- Add BSD license for the `.ReduceAmp` function.

# wrv 0.8.0

- Reorganize model archive to comply with new open data requirements.

- Remove suggested **git2r** R-package, version control is no longer used when constructing the model archive.

- Add USGS software disclaimer to the package startup message.

- Fix bug that prevented package compilation on R version 3.3.0.

- Address editorial review comments in package vignettes.

# wrv 0.7.2

- Change LaTeX bibliography engine from natbib to biblatex.

# wrv 0.7.1

- Fix bug with scientific-notation format of axis-tickmark labels in the PlotGraph function.

- Calculate the relative composite sensitivity in the sensitivity dataset.

# wrv 0.7.0

- Vignettes ready for editorial review.

- Update PEST sensitivity dataset with results from final model calibration run.

# wrv 0.6.4

- All colleague review comments pertaining to package vignettes have been addressed.

- Fix bug in PlotMap function that was introduced with **sp** version 1.2-2.
  Function `sp::gridlines` now returns the correct ID attribute order for labels "EW" and "NS".

- Change default value for `auto.flow.reduce` argument in the `WriteModflowInput` function to `FALSE`.

# wrv 0.6.3

- For sampling purposes the `BumpRiverStage` function requires a random number generator (RNG).
  The RNG state is now set using the set.seed function and results are reproducible.

- Add make.intervals argument to the PlotMap function.

# wrv 0.6.2

- Add auto.flow.reduce argument to the WriteModflowInput function, its default is `TRUE`.
  The MODFLOW-USG option "AUTOFLOWREDUCE" is now set for all model formulations, not just the "convertible" formulation.
  Pumping rates that have been automatically reduced will be written to a model output file ("*.afr").

- Shorten a few of the long function names in order to tidy the manual vignette.

- Add PEST sensitivity dataset, this will be updated after final calibration.
  Plotting routines for this dataset were added to the miscellaneous vignette.

- Fix minor bug with PlotCrossSection function that may have prevented improper cell coloring.

# wrv 0.6.1

- Use more robust estimate of riverbed conductance.

- Change number of time steps per stress period to 4, previously was 1.

- Change format for package version numbering from #.#-# to #.#.#

# wrv 0.6-0

- Fix bug in steady-state calculation using the `ss.interval` argument in the `UpdateWaterBudget` function.

# wrv 0.5-6

- Add `iwelcb` argument to the `UpdateWaterBudget` function.

- Require R version greater than or equal to 3.2, was previously 3.1.

- Change `UpdateWaterBudget` function to write well configuration data to the "qa-well-config.csv" file.

- Rename exported CSV files from "wrv-summary-*.csv" to "qa-*.csv", where the "qa" stands for quality assurance.

- Add `ss.interval` argument to the `UpdateWaterBudget` function.
  Specifying this interval overrides the default time period representing steady-state boundary conditions.

# wrv 0.5-5

- River cells in lower model layers have been removed.
  Cell geometries were changed to compensate for river stage elevations located beneath the bottom of a river cell.

# wrv 0.5-3

- The criterion for selecting lower-layer river cells has changed from riverbed bottom to river stage.

# wrv 0.5-2

- Consolidate ./inst/templates and ./inst/references folder files in ./inst/misc folder, and add README file.

- Remove links to external package functions in manual.
  This prevents broken links in manual vignette.

- Add Rd2.tex file to facilitate building of vignette manual, see `Rd2pdf` command.

- Add model.area field to "wrv-summary-incidental.csv"; see `RunWaterBalanceModel` function.

# wrv 0.5-0

- Change `UpdateWaterBudget` function argument to allow toggling of output units in summary tables.

- Change file format for "wrv-summary-pumping.csv".

- Rename vignettes to reflect report appendices.

- Add suggested number of axis intervals as new arguments in the `PlotGraph` function.

- Add `ToLatexScientific` function to format scientific notation as m x 10^n.

- Add `subreach.recharge` dataset, used to calculate stream-aquifer flow-exchange ratios.

- Revise `GetWellConfig` function to be more general; although, it's still hard-coded for three model layers.

- Changed MODFLOW solver options NONLINMETH and IACL to 1 and 0 (were previously 2 and 2), respectively.
  These new solver options coincide with the current PEST run.

- Fixed minor bug in `ReadModflowBinaryFile` function that was causing it to get stuck in a continuous loop when a variable description was not recognized.

# wrv 0.4-6

- Change October streamflows in the "Big Wood, Wood River Ranch to Stanton Crossing" reach to non-dry-bed conditions.

# wrv 0.4-5

- Add dry-bed conditions to the "Big Wood, Wood River Ranch to Stanton Crossing" reach.

- Add new vignette titled "Parameter Estimation of the Groundwater Flow Model".

- The **viridis** and **git2r** R-packages are now suggested to improve plot colors and add version control to the model archive, respectively.

# wrv 0.4-4

- Update tributary underflow datasets to account for the raw-data measurement resolution and the addition of a new tributary boundary at Oregon Gulch.

- Add `AddInsetMap` function to draw an inset map in the current graphics device.

- The R package **animation** is now suggested and used to create animations showing the depth-to-water over time.

# wrv 0.4-3

- The Hailey precipitation data was found to be much less than what is shown in the design document.
  All precipitation data was recalculated from raw data and validated with the design document.

- Add new vignette titled "Natural Groundwater Recharge".

- Renamed vignette files.

# wrv 0.4-2

- Fix error associated with the units of soil percolation rate; units changed from meters per day to meters per month.

- Fix minor bug in the `SetPolygons` function that may have interfered with polygon intersections.

# wrv 0.4-1

- Add new vignette titled "Incidental Recharge and Groundwater Pumping Demand".

- Annual flow rate in the "BWR Upper" tributary canyon was changed from 145 acre-feet to 602 acre-feet.

- Add `PlotCrossSection`, `AddColorKey`, and `AddScaleBar` functions.

- Fix transient stream stage on Big Wood River between Hailey and Stanton Crossing streamgages.

- Reference data filenames were changed back to "EFF.csv", "TRIB.csv", and "SEEP.csv".

# wrv 0.4-0

- New package vignette "wrv-process" that begins to document the instructions for processing the calibrated model.

- Reference data filenames were changed from "EFF.csv", "TRIB.csv", "SEEP.csv" to "EFF.ref", "TRIB.ref", "SEEP.ref", respectively.

- Batch filenames were changed from "Run.bat", "Update.bat" to "RunModflow.bat", "UpdateBudget.bat", respectively.

- Output from "wrv-model" vignette adheres to the recommended USGS model archive structure.

- Add "README.md" to the "./inst/extdata/" folder; the files and folders were reorganized under this folder.

- MODFLOW-USG (version 1.2) source code is included in the package source.
  A MODFLOW-USG executable is included with the precompiled binary versions of the package.

# wrv 0.3-0

- Add content to post-processing analysis of transient model simulation in the wrv-model vignette.

- Improve performance for reading MODFLOW binary flow data in the ReadModflowBinaryFile function.

- Add SummariseBudget function to summarize cell-by-cell flow for model boundary components.

- R-package **sfsmisc** is now suggested to improve figure axes labels.

# wrv 0.2-7

- Write volumetric budget for every stress period, was previously only written for the last stress period.

-  R-package **dplyr** is now required.

- Change from using `base::merge` and `stats::aggregate` functions to **dplyr** equivalents.
  Use of `dplyr::left_join` function will fix bug with `base::merge` auto-arranging row order.

- Remove perianal dry-bed conditions for the "Big Wood, Wood River Ranch to Stanton Crossing" river reach.

# wrv 0.2-6

- Normalize the Big Wood River gage-height using median rather than mean.
  This prevents stream-stage elevations from being lower than streambed elevations.

# wrv 0.2-5

- Change calculation for missing gage-height data at the USGS 13140800 streamgage.

# wrv 0.2-4

- Modeling transient stage conditions in the Big Wood River.

- Change option for reading hydraulic parameters in the model.
  The model now expects Specific Storage rather than the Storage Coefficient.

- Tidy help documentation.

# wrv 0.2-3

- Improve computational efficiency of `RunWaterBalanceModel` function.

- The `UpdateWaterBudget` function call in Update.bat now passes the default local directory.

# wrv 0.2-2

- Account for recharge from miscellaneous seepage sites (Bypass Canal and Bellevue WWTP Ponds).

- Add support for writing raster stack to disk as a KLM file.

- Update numerous raw data files.

# wrv 0.2-1

- Account for episodic dry periods in the stream reaches located between Glendale and Wood River Ranch.
  Requires passing river data as a `data.frame-class` object to the `WriteModflowInputFiles` function.

- Update `SetPolygons` function to handle point and line artifacts.

- Tidy code for `UpdateWaterBudget` function and add output that is more descriptive.

- Update numerous raw data files.

- Revise area calculation in summary table for recharge on non-irrigated lands.

# wrv 0.2-0

- Add `UpdateWaterBudget` function; only used during parameter estimation.

- Add model options for (1) steady-state or transient conditions, (2) confined or convertible conditions, and (3) verbose output.

- All areal recharge and specified flows in the major tributary canyons are placed in the MODFLOW Well Package.
  The MODFLOW Recharge Package and MODFLOW Flow and Head Boundary Package are no longer in use.

- Update numerous raw data files.

- Fix bug with calculation of recharge on non-irrigated lands.

- Revise calculation of river stage and river bottom elevations.

# wrv 0.1-5

- Remove dependence on **png** package.

- Components of recharge and pumping added to model.

- The `FindConnectedCells` function was replaced by a new `BumpDisconnectedCells` function.
  Model cells producing vertical disconnects between adjacent cells are no longer removed.

- Many changes and additions to raw data files and package datasets.

- Revise method for calculating groundwater inflow in the tributary canyons.
  The `GetSesonalMultiplier` function was added to assist with the temporal distribution of flow in the tributary canyons.

- The `DownloadFile` function attempts to re-download a file if the previous attempt fails.

# wrv 0.1-4

- Add an additional hydrogeologic zone that is composed entirely of the alluvium unit under confined conditions.
  The horizontal hydraulic conductivity for all hydrogeologic zones has been changed to better reflect values given by Bartolino and Adkins (2012, pg. 25-26).

- New package vignette that documents the construction of package datasets.

- There is no longer the need to specify the number of bytes when reading MODFLOW binary files in the `ReadModflowBinaryFile` function.

# wrv 0.1-3

- Add `ReadModflowListFile` function to read and parse volumetric budget information from the MODFLOW listing file.

- Simulate stream-aquifer flow exchange in the Big Wood River and Silver Creek.

- Add `SetPolygons` function to determine the intersection or difference between two multi-polygon objects.

# wrv 0.1-2

- First public release coinciding with Modeling Technical Advisory Committee meeting Dec. 2013.
