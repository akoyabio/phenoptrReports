# phenoptrReports <img src="man/figures/Akoya.png" align="right" height="50px" />

## Introduction

`phenoptrReports` generates reports based on data
created by Akoya Biosciences' inForm<sup>&reg;</sup> software.

### Merging data from individual fields

The "Merge cell seg files" application merges inForm data files from individual
fields to create "Merge" data files. This is similar to the inForm Merge tab but
does not include the ability to review and reject individual fields.

### Data aggregation and reporting

The consolidation and analysis applications in
`phenoptrReports` use merged data files to 
analyze cell phenotype and expression data
created by inForm. They use data from multiple inForm projects
to aggregate phenoype counts, density, 
mean expression and
H-Score for each slide and tissue category in an experiment. 

For each analysis, the applications create 

- a consolidated data file
- an Excel workbook with aggregation results
- a Word document with visualizations of the results
- an R script which provides
  an audit trail and can be used to reproduce or modify the analysis.

### Unmixing quality report

The unmixing quality report analyzes unmixed, singleplex images
to help evaluate staining and unmixing quality. This report
shows crosstalk between components and
highlights potential problem areas in assay development.

### Component levels report

The component levels report analyzes unmixed, multiplex images to help
evaluate staining levels for an entire experiment. This report shows
the distribution of signal and dark pixels for all components.

----

`phenoptrReports` is part of the Akoya Biosciences Phenoptics&trade; family of
Quantitative Pathology Research Solutions. For more information
visit the Phenoptics&trade;
[home page](http://www.perkinelmer.com/category/quantitative-pathology-research).

----

## Installation

`phenoptrReports` requires the [R environment](https://www.r-project.org/) 
for statistical computing, version 3.5.0 or higher. To install R,
visit the [R download](https://cloud.r-project.org/) page.

The analysis apps in `phenoptrReports` run as "Addins" to
the [RStudio IDE](https://www.rstudio.com/products/rstudio/), which must also
be installed.

1. Install R. Download the most recent version from  https://cloud.r-project.org/.
1. Install RStudio. Download the desktop version from https://www.rstudio.com/products/rstudio/.
1. Start RStudio.
1. Install `phenoptrReports` from GitHub. In the RStudio console, 
copy and paste or type these commands (press Enter after each line):
```
install.packages("devtools")
devtools::install_github("akoyabio/phenoptrReports")
```
1. If requested, enter `1` (Yes) to install BiocInstaller.
1. Restart RStudio.

----

## Getting Started

### Merging data from individual fields

Use the "Merge cell seg files" addin to merge inForm data from individual fields
into "Merge" data files. This is similar to the function of the inForm Merge
tab and should be used only when the inForm Merge is not available.

### Data consolidation

A `phenoptrReports` data analysis has two major steps - data consolidation and data
aggregation. Start the consolidation step by selecting **Consolidate and summarize** from
the RStudio Addins menu.

The consolidation step combines the output from multiple inForm projects into a 
single consolidated data file, creating columns for each individual
phenotype. The inputs to this step are merged cell seg
data files created by inForm. The output is a consolidated data file and a
summary report for each file. 

Run the consolidation step even if your data comes from a single
inForm project. This creates a data file in the format that the analysis
step uses.

For detailed instructions and requirements, see the 
[Consolidating inForm data](https://akoyabio.github.io/phenoptrReports/articles/consolidation.html) tutorial.

### Data analysis

The analysis step has three parts: 

- select the input files
- define the desired analysis
- create the final reports

The most important input file is the consolidated data file from
the consolidation step. Selecting a summary cell seg data file enables
density calculations; selecting a score data file enables H-Score calculation.

To define the analysis, you select tissue categories, phenotypes, and markers of 
interest.

To create the final reports, the analysis app writes and runs an R script.
The script computes the aggregated statistics, writes them to an Excel
workbook, and creates visualizations of the results in a Word document.

For detailed instructions and requirements, see the 
[Analyzing inForm data](https://akoyabio.github.io/phenoptrReports/articles/analysis.html) tutorial.


### Unmixing quality report

For more information about the unmixing quality report, see the 
[Unmixing quality report](https://akoyabio.github.io/phenoptrReports/articles/unmixing_quality_report.html) tutorial.
