# Globals for analysis_app
library(tidyverse)
library(phenoptr)
library(phenoptrReports)
library(shiny)

base_dir = getwd() # For development
#base_dir = system.file('analysis_app', package='phenoptrReports') # For release
source(file.path(base_dir, 'phenotype_module.R'))
source(file.path(base_dir, 'files_module.R'))
source(file.path(base_dir, 'formatters.R'))
source(file.path(base_dir, 'helpers.R'))
