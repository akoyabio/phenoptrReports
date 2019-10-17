# Globals for spatial viewer app
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

base_dir = system.file('spatial_map_viewer_app', package='phenoptrReports')
source(file.path(base_dir, 'phenotype_color_module.R'))

# .csd_path and .export_path must be available in the environment.
# Normally they are set by `spatial_map_viewer` which launches this app.
delim = ifelse(endsWith(.csd_path, 'csv'), ',', '\t')
csd = vroom::vroom(.csd_path, na='#N/A', delim=delim, col_types=vroom::cols())

if (any(grepl('pixel', names(csd))))
  stop('spatial map viewer requires data in microns.')

available_phenotypes = phenoptr::unique_phenotypes(csd) %>%
  purrr::map_chr(~stringr::str_remove(.x, '\\+$'))

available_fields = sort(unique(csd[[phenoptr::field_column(csd)]]))

# Create a temp dir to save image files in
temp_dir = file.path(tempdir(), 'image')
dir.create(temp_dir, showWarnings=FALSE)
shiny::addResourcePath('image', temp_dir)
