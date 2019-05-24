# Globals for spatial viewer app
library(dplyr)
library(ggplot2)

base_dir = system.file('spatial_map_viewer_app', package='phenoptrReports')
source(file.path(base_dir, 'phenotype_color_module.R'))

# .nn_path and .export_path must be available in the environment
# .nn_path = "C:\\Research\\phenoptrReports\\tests\\testthat\\results\\nearest_neighbors.csv"
# .export_path = 'C:\\Research\\phenoptrExamplesData\\DemoSlides\\export'

csd = readr::read_csv(.nn_path, na='#N/A', col_types=readr::cols())
available_phenotypes = names(csd) %>%
  stringr::str_subset('Phenotype ') %>%
  stringr::str_remove('Phenotype ')

# Check that we have distance and Cell ID columns for all phenotypes
required_columns = c(paste0('Distance to ', available_phenotypes, '+'),
                     paste0('Cell ID ', available_phenotypes, '+'))
missing_columns = setdiff(required_columns, names(csd))
if (length(missing_columns) > 0)
  stop('Some required columns are not available: ',
       paste(missing_columns, collapse=', '))

available_fields = sort(unique(csd[[phenoptr::field_column(csd)]]))
