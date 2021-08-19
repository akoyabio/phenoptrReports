# phenoptrReports <img src="man/figures/Akoya.png" align="right" width="175" />

## Reports and visualizations from inForm data

`phenoptrReports` generates reports and visualizations based on data
created by Akoya Biosciences' inForm<sup>&reg;</sup> software.

`phenoptrReports` is part of the Akoya Biosciences Phenoptics&trade; family of
Quantitative Pathology Research Solutions. For more information visit the 
[Phenoptics&trade; home page](https://www.akoyabio.com/phenopticstm/technology/quantitative-pathology-research).

----

## Installation

The analysis apps in `phenoptrReports` run as "Addins" to
the [RStudio IDE](https://www.rstudio.com/products/rstudio/), which must also
be installed.

`phenoptrReports` uses the Akoya Biosciences `phenoptr` package for much of 
its functionality. Please follow the 
[`phenoptr` installation instructions](https://akoyabio.github.io/phenoptr/#installation),
including the optional installations, before installing `phenoptrReports`.

Next, install `phenoptrReports` from GitHub. In the RStudio console, 
copy and paste or type this command:
```
remotes::install_github("akoyabio/phenoptrReports")
```

----

## Getting Started

`phenoptrReports` functionality is provided via RStudio Addins. These are 
small applications accessed via the "Addins" menu in the RStudio IDE.

### Merge data from individual fields

The **Merge cell seg files** addin merges inForm data files from individual
fields to create "Merge" data files. This is similar to the inForm Merge tab but
does not include the ability to review and reject individual fields. Use this
addin if you are not able to merge in inForm for any reason.

This addin allows you to select an inForm export directory containing data files
for individual fields. It creates merge data files combining data for all fields.

### Consolidate merged data to a single file

The **Consolidate and summarize** addin combines the output from multiple
inForm projects into a 
single consolidated data file, creating columns for each individual
phenotype. The inputs to this addin are merged cell seg
data files created by inForm. The output is a consolidated data file and a
summary report for each file. 

Optionally, this addin will include, exclude and tag cells according to
regions of interest created in Phenochart.

Run the consolidation addin even if your data comes from a single
inForm project. This creates a data file in the format that the analysis
addin uses.

For detailed instructions and requirements, see the 
[Consolidating inForm data tutorial](https://akoyabio.github.io/phenoptrReports/articles/consolidation.html).

### Data analysis

The **Analyze consolidated data** addin reads data produced by
the consolidation addin. It aggregates phenoype counts, density, 
mean expression and
H-Score for each slide and tissue category in an experiment and can also
report on nearest neighbors and count cells within a radius.

This addin creates

- an Excel workbook with aggregation results
- a Word document with visualizations of the results
- CSV files containing spatial metrics
- an R script which provides
  an audit trail and can be used to reproduce or modify the analysis.

The analysis addin has three parts: 

- select the input files
- define the desired analysis
- create the final reports

The most important input file is the consolidated data file from
the consolidation addin. Selecting a summary cell seg data file enables
density calculations; selecting a score data file enables H-Score calculation.

To define the analysis, you select tissue categories, phenotypes, and markers of 
interest.

To create the final reports, the analysis app writes and runs an R script.
The script computes the aggregated statistics, writes them to an Excel
workbook, and creates visualizations of the results in a Word document.

For detailed instructions and requirements, see the 
[Analyzing inForm data tutorial](https://akoyabio.github.io/phenoptrReports/articles/analysis.html).

### Visualization of spatial relationships

The **Spatial map viewer** addin creates visualizations of nearest neighbors of 
selected phenotypes within individual fields. The visualizations can be saved 
for use in other applications.

The spatial map viewer requires a `Consolidated_data.txt` file from the
consolidation app or the `nearest_neighbors.csv` file created by
the analysis addin, and an inForm image directory containing composite and
component images.

For detailed instructions and requirements, see the 
[Visualizing spatial relationships tutorial](https://akoyabio.github.io/phenoptrReports/articles/spatial_map_viewer.html).

### Unmixing quality report

The **Unmixing quality report** addin analyzes unmixed, singleplex images
to help evaluate staining and unmixing quality. This report
shows crosstalk between components and
highlights potential problem areas in assay development.

For more information about the unmixing quality report, see the 
[Unmixing quality report tutorial](https://akoyabio.github.io/phenoptrReports/articles/unmixing_quality_report.html).

### Component levels report

The **Component levels report** addin analyzes unmixed, multiplex images to help
evaluate staining levels for an entire experiment. This report shows
the distribution of signal and dark pixels for all components.

For more information about the component levels report, see the 
[Component levels report tutorial](https://akoyabio.github.io/phenoptrReports/articles/component_levels_report.html).

### Mean of Top 20/ bottom 10 report

The **Mean of Top 20/ bottom 10** analysis helps to evaluate whether the staining quality is likely to produce good unmixing of the markers in the image. The analysis computes the mean expression of selected markers in the 20 highest-expressing cells and the 10% lowest-expressing cells.

For detailed instructions and requirements, see the
[Mean of top 20 / bottom 10 cells tutorial](https://akoyabio.github.io/phenoptrReports/articles/top_20_bottom_10_report.html).

### Staining consistency report

The **Staining consistency report** measures variation in the mean expression
of a single marker across multiple images. It is used to assess consistency
of staining across multiple staining runs.

For detailed instructions and requirements, see the
[Staining consistency report tutorial](https://akoyabio.github.io/phenoptrReports/articles/staining_consistency_report.html).

<!-- badges: start -->
[![R-CMD-check](https://github.com/akoyabio/phenoptrReports/workflows/R-CMD-check/badge.svg)](https://github.com/akoyabio/phenoptrReports/actions)
<!-- badges: end -->
