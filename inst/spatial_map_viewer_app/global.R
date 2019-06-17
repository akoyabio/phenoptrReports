# Globals for spatial viewer app
library(dplyr)
library(ggplot2)

base_dir = system.file('spatial_map_viewer_app', package='phenoptrReports')
source(file.path(base_dir, 'phenotype_color_module.R'))

# .nn_path and .export_path must be available in the environment
# .nn_path = "C:\\Research\\phenoptrReports\\tests\\testthat\\results\\nearest_neighbors.csv"
# .export_path = 'C:\\Research\\phenoptrExamplesData\\DemoSlides\\export'

csd = readr::read_csv(.nn_path, na='#N/A', col_types=readr::cols())

# The available phenotypes are ones for which we have both
# phenotype and Cell ID <phenotypes> columns.
# We don't use the Distance to <phenotype> columns and we don't support
# multiple phenotypes.
available_phenotypes = names(csd) %>%
  stringr::str_subset('Phenotype ') %>%
  stringr::str_remove('Phenotype ')

distance_to_phenotypes = names(csd) %>%
  stringr::str_subset('Distance to ') %>%
  stringr::str_remove('Distance to ') %>%
  stringr::str_remove('\\+$')

available_phenotypes = intersect(available_phenotypes, distance_to_phenotypes)

available_fields = sort(unique(csd[[phenoptr::field_column(csd)]]))
