# Helpers for the spatial map viewer app

#' Make a nearest neighbor map for a single field
#'
#' The phenotype definitions may be NA, in which case the base field
#' will be shown with any available phenotype.
#'
#' Note: Only single phenotypes are supported.
#' @param csd Cell seg data with distance columns
#' @param field Sample Name or Annotation ID to map
#' @param export_path Path to a directory containing composite and component
#' image files from inForm
#' @param pheno1,pheno2 Phenotype definitions
#' @param color1,color2 Colors to draw the phenotype dots
#' @param show_as Which nearest neighbors should be shown?
#' @param dot_size Size of the dots used to show phenotypes
#' @return A ggplot object
#' @export
nearest_neighbor_map =
  function(csd, field, export_path,
           pheno1, pheno2, color1, color2,
           show_as=c('from_to', 'to_from', 'mutual', 'none'),
           dot_size=3) {
  field_data = csd[csd[[phenoptr::field_column(csd)]]==field,]
  field_info = read_field_info(field, export_path)
  if (is.null(field_info)) {
    warning('No component image available for ', field, ', skipping')
    return()
  }

  # Workaround for inForm data that was originally pixels. In that case
  # field_data will have the origin at top left; convert to slide origin
  # to match the field_info.
  if (max(field_data$`Cell X Position`) < field_info$field_size[1]
      || max(field_data$`Cell Y Position`) < field_info$field_size[2]) {
    field_data = field_data %>% dplyr::mutate(
      `Cell Y Position` = `Cell Y Position`+field_info$location[2],
      `Cell X Position` = `Cell X Position`+field_info$location[1]
    )
  }

  background = read_background(field, export_path)

  # Make a base plot
  xlim=c(field_info$location[1],
         field_info$location[1]+field_info$field_size[1])
  ylim=c(field_info$location[2],
         field_info$location[2]+field_info$field_size[2])

  colors = c(color1, color2) %>%
    rlang::set_names(c(pheno1, pheno2))

  base_plot = ggplot(mapping=aes(x=`Cell X Position`,
                                 y=`Cell Y Position`)) %>%
    phenoptr:::add_scales_and_background(background, xlim, ylim,
                                         scale_color='white') +
    labs(x='Cell X Position', y='Cell Y Position') +
    scale_color_manual('Phenotype', values=colors)


  # Filter to just relevant phenotypes
  pheno1_cells = if (is.na(pheno1)) NULL else field_data %>%
    dplyr::filter(phenoptr::select_rows(field_data, pheno1))

  pheno2_cells = if (is.na(pheno2)) NULL else field_data %>%
    dplyr::filter(phenoptr::select_rows(field_data, pheno2))

  # Showing nearest neighbors requires two phenotypes
  if (!is.na(pheno1) && !is.na(pheno2)) {
    if (show_as=='from_to') {
      # For each pheno1 cell, join with the data for the nearest pheno2 cell
      pheno1_to_pheno2 = match_cells(pheno1_cells, pheno2_cells, pheno2)

      # Add lines
      p = base_plot + geom_segment(data=pheno1_to_pheno2,
                                   aes(xend=`Cell X Position.to`,
                                       yend=`Cell Y Position.to`),
                                   color='white') +
        labs(title=paste0(field, ' - Nearest ', pheno2, ' to each ', pheno1))
    }
    else if (show_as=='to_from') {
      # for each pheno2 cell, find the nearest pheno1 cell
      pheno2_to_pheno1 = match_cells(pheno2_cells, pheno1_cells, pheno1)

      p = base_plot +
        geom_segment(data=pheno2_to_pheno1,
                     aes(xend=`Cell X Position.to`,
                         yend=`Cell Y Position.to`),
                     color='white') +
        labs(title=paste0(field, ' - Nearest ', pheno1, ' to each ', pheno2))
    }
    else if (show_as=='mutual') {
      # Mutual nearest neighbors
      # Mutual nearest neighbors are cells which have each other as nearest
      # neighbors; i.e. cells where the nearest neighbor of the nearest neighbor
      # is the starting cell.

      pheno1_to_pheno2 = match_cells(pheno1_cells, pheno2_cells, pheno2)
      match_col = paste0('Cell ID ', pheno1, '.to') %>% rlang::sym()
      mutual = pheno1_to_pheno2 %>%
        filter(`Cell ID`==!!match_col)

      p = base_plot +
        geom_segment(data=mutual,
                     aes(xend=`Cell X Position.to`, yend=`Cell Y Position.to`),
                     size=1, color='white') +
        labs(title=paste0(field, ' - Mutual nearest neighbors - ',
                          pheno1, ' and ', pheno2))
    } else {
      # Don't show nearest neighbors, just cells
      p = base_plot +
        labs(title=paste0(field, ' - ', pheno1, ' and ', pheno2))
    }
  } else {
    # One or both phenotypes are missing, just show the field name as title
    p = base_plot + labs(title=field)
  }

  # We want the points on top of the lines, so add them last
  if (!is.na(pheno1))
    p = p + geom_point(data=pheno1_cells, aes(color=pheno1), size=dot_size)
  if (!is.na(pheno2))
    p = p + geom_point(data=pheno2_cells, aes(color=pheno2), size=dot_size)

  # A little theming
  p + theme(legend.key = element_rect(fill = "white"),
            legend.position='bottom') +
    guides(color = guide_legend(override.aes = list(size = 5)))
}

# Join from_cells and to_cells by nearest neighbor cell ID
match_cells = function(from_cells, to_cells, to_name) {
  to_id_col = paste0('Cell ID ', to_name) # Column with to_name IDs
  by = rlang::set_names('Cell ID', to_id_col)
  from_cells %>%
    dplyr::left_join(to_cells, by=by, suffix=c('', '.to'))
}

# Try to read a composite image for a field
read_background = function(field, export_path) {
  # Field can be an Annotation ID or Sample Name
  # If it is a sample name, remove the .im3 suffix
  field_base = stringr::str_remove(field, '\\.im3')

  # Look for a composite image
  image_endings = c(
    '_composite_image.tif',
    '_composite_image.jpg'
  )
  for (ending in image_endings) {
    background_path = file.path(export_path, paste0(field_base, ending))
    if (file.exists(background_path)) break
  }

  # Read the image and convert to raster
  if (file.exists(background_path))
  {
    if (grepl('jpg$', background_path))
      background = jpeg::readJPEG(background_path)
    else background = tiff::readTIFF(background_path)

    background = as.raster(background)
  } else {
    background = NULL
  }
  background
}

# Get field metadata from the component data file
read_field_info = function(field, export_path) {
  field_base = stringr::str_remove(field, '\\.im3')
  component_path = file.path(export_path, paste0(field_base, '_component_data.tif'))
  if(!file.exists(component_path))
    return(NULL)

  field_info = phenoptr::get_field_info(component_path)
  field_info$pixels_per_micron = 1/field_info$microns_per_pixel
  field_info
}
