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

available_hashtags = names(csd) %>% stringr::str_subset('^#')

# Open one composite to figure out the available views
composite_path = list.files(.export_path, pattern='composite_image\\.',
                            full.names=TRUE, recursive=TRUE)[1]
if (is.na(composite_path))
  stop('No composite images found in ', .export_path)

available_views = c(Standard=1) # Default if no info in composite image

# Look for multiple, named composites in a TIFF image
if (endsWith(composite_path, 'tif')) {
  info = phenoptr::read_composite_info(composite_path)

  if (length(info) > 0 && length(info[[1]]) > 0)
    available_views = seq_along(info) %>%
      setNames(purrr::map_chr(info, 'composite_name'))
}

# Create a temp dir to save image files in
temp_dir = file.path(tempdir(), 'image')
dir.create(temp_dir, showWarnings=FALSE)
shiny::addResourcePath('image', temp_dir)
