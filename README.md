# phenoptrReports <img src="man/figures/Akoya.png" align="right" height="50px" />

## Data aggregation and reporting for inForm data

`phenoptrReports` analyzes cell phenotype and expression data
created by Akoya Biosciences' inForm<sup>&reg;</sup> software. 

Applications in this package use data from multiple inForm projects
to aggregate phenoype counts, density, 
mean expression and
H-Score for each slide and tissue category in an experiment. 

For each analysis, the applications create 

- a consolidated data file
- an Excel workbook with aggregation results
- a Word document with visualizations of the results
- an R script which provides
an audit trail and can be used to reproduce or modify the analysis.

`phenoptrReports` is part of the Akoya Biosciences Phenoptics&trade; family of
Quantitative Pathology Research Solutions. For more information
visit the Phenoptics&trade;
[home page](http://www.perkinelmer.com/cancer-immunology/index.html).

----

## Installation

`phenoptrReports` requires the [R environment](https://www.r-project.org/) 
for statistical computing, version 3.3.0 or higher. To install R,
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


A `phenoptrReports` analysis has two major steps - data consolidation and data
aggregation.

### Data requirements

`phenoptrReports` creates reports based on inForm output files.

#### Tissue segmentation

inForm projects used with `phenoptrReports` must include a tissue 
segmentation step.

#### Phenotypes

The names of positive phenotypes used with `phenoptrReports` must end
with "+", for example "CD8+" or "FoxP3+". Negative and "Other"
phenotype names should not end in "+".

#### Merged cell seg data

The primary source data for `phenoptrReports` is 
one or more merged cell seg data files created in the inForm merge step.
You may use multiple merge files from multiple inForm projects with different 
phenotypes. The Slide ID and Cell ID fields in multiple merge files 
must agree exactly. 

#### Other files

Two other input files are optional. 

If provided, `phenotprReports` will use a merged cell seg summary file
to compute tissue area and cell density within tissue categories. 
A score data summary file is used to compute H-Score per slide and 
tissue category.

### Data consolidation

Start the consolidation step by selecting **Consolidate and summarize** from
the RStudio Addins menu.

The consolidation step combines the output from multiple inForm projects into a 
single consolidated data file, creating columns for each individual
phenotype. The inputs to this step are merged cell seg
data files created by inForm. The output is a consolidated data file and a
summary report for each file. 

Run the consolidation step even if your data comes from a single
inForm project. This creates a data file in the format that the analysis
step uses.

For detailed instructions, see `vignette('consolidation')`.

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

To create the finalreports, the analysis app writes and runs an R script.
The script computes the aggregated statistics, writes them to an Excel
workbook, and creates visualizations of the results in a Word document.

For detailed instructions, see `vignette('analysis')`.

