# Globals for spatial viewer app
library(dplyr)
library(ggplot2)

base_dir = system.file('spatial_map_viewer_app', package='phenoptrReports')
source(file.path(base_dir, 'phenotype_color_module.R'))

# .csd_path and .export_path must be available in the environment
# .csd_path = "C:\\Research\\phenoptrReports\\tests\\testthat\\results\\nearest_neighbors.csv"
# .export_path = 'C:\\Research\\phenoptrExamplesData\\DemoSlides\\export'

csd = if (endsWith(.csd_path, 'csv')) {
  readr::read_csv(.csd_path, na='#N/A', col_types=readr::cols())
  } else {
    readr::read_tsv(.csd_path, na='#N/A', col_types=readr::cols())}

available_phenotypes = phenoptr::unique_phenotypes(csd) %>%
  purrr::map_chr(~stringr::str_remove(.x, '\\+$'))

available_fields = sort(unique(csd[[phenoptr::field_column(csd)]]))
