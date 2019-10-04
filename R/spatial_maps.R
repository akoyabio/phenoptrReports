# Helpers for the spatial map viewer app

# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(
  c('Cell X Position.to', 'Cell Y Position.to'))

#' Make a nearest neighbor map for a single field
#'
#' The phenotype definitions may be NA, in which case the base field will be
#' shown with any available phenotype.
#'
#' @param csd Cell seg data with distance columns
#' @param field_name Sample Name or Annotation ID to map
#' @param export_path Path to a directory containing composite and component
#'   image files from inForm
#' @param phenos Named list of phenotype definitions. Must have length 2.
#' @param color1,color2 Colors to draw the phenotype dots
#' @param show_as Which nearest neighbors should be shown?
#' @param dot_size Size of the dots used to show phenotypes
#' @param add_logo Show the Akoya logo in the image?
#' @return Returns a `list` containing two items: \describe{ \item{`plot`}{The
#'   plot, a \code{\link[ggplot2]{ggplot}} object.} \item{`data`}{A
#'   \code{\link[tibble]{tibble}} containing the data used to create the line
#'   segments in the plot, or `NULL` if `show_as` is `"none"`.
#'   Columns with the suffix `.to` contain data for the "to" cells.} }
#' @export
nearest_neighbor_map =
  function(csd, field_name, export_path,
           phenos, color1, color2,
           show_as=c('from_to', 'to_from', 'mutual', 'none'),
           dot_size=3, add_logo=TRUE) {
  stopifnot(is.list(phenos) &&length(phenos) == 2)

  # Get the phenotype definitions
  phenos = phenoptr::validate_phenotypes(phenos, csd)
  pheno1 = phenos[[1]]
  have_pheno1 = !any(is.na(pheno1))
  pheno_name1 = names(phenos)[[1]]

  pheno2 = phenos[[2]]
  have_pheno2 = !any(is.na(pheno2))
  pheno_name2 = names(phenos)[[2]]

  # Subset csd to just the cells of interest
  field_data = csd[csd[[phenoptr::field_column(csd)]]==field_name,]
  selected_cells = phenoptr::select_rows(field_data, pheno1) |
    phenoptr::select_rows(field_data, pheno2)
  field_data = field_data[selected_cells, ]

  # Make sure we have the necessary distance columns
  if (show_as != 'none' && have_pheno1 && have_pheno2)
    field_data = ensure_distance_columns(field_data, phenos)

  # Get spatial reference info from the component image
  field_info = read_field_info(field_name, export_path)
  if (is.null(field_info)) {
    warning('No component image available for ', field_name, ', skipping.')
    return()
  }

  # Workaround for inForm data that was originally pixels. In that case
  # field_data will have the origin at top left; convert to slide origin
  # to match the field_info.
  # Assume that the data was converted with the default 2 pixels per micron.
  # Test for all cells in a field fitting within the field size after
  # correcting the location.
  correction = 2 / field_info$pixels_per_micron
  if (max(field_data$`Cell X Position`)*correction < field_info$field_size[1]
      && max(field_data$`Cell Y Position`)*correction < field_info$field_size[2]) {
    field_data = field_data %>% dplyr::mutate(
      `Cell Y Position` = `Cell Y Position`*correction+field_info$location[2],
      `Cell X Position` = `Cell X Position`*correction+field_info$location[1]
    )
  }

  # Filter to just relevant from & to phenotypes
  pheno1_cells = if (!have_pheno1) NULL else field_data %>%
    dplyr::filter(phenoptr::select_rows(field_data, pheno1))

  pheno2_cells = if (!have_pheno2) NULL else field_data %>%
    dplyr::filter(phenoptr::select_rows(field_data, pheno2))

  # Start making the plot
  background = read_background(field_name, export_path)

  # Make a base plot
  xlim=c(field_info$location[1],
         field_info$location[1]+field_info$field_size[1])
  ylim=c(field_info$location[2],
         field_info$location[2]+field_info$field_size[2])

  colors = c(color1, color2) %>%
    rlang::set_names(c(pheno_name1, pheno_name2))

  base_plot = ggplot2::ggplot(mapping=ggplot2::aes(x=`Cell X Position`,
                                 y=`Cell Y Position`)) %>%
    phenoptr:::add_scales_and_background(background, xlim, ylim,
                                         scale_color='white') +
    ggplot2::labs(x='Cell X Position', y='Cell Y Position') +
    ggplot2::scale_color_manual('Phenotype', values=colors)

  p = base_plot

  if (add_logo) {
    # Add the Akoya logo beneath the scale bar
    # We will do this twice. The first time is solid white, drawn over
    # the background and under the dots. The second time will be
    # over the dots and partly transparent.
    logo_path = system.file('etc',
                            'AKOYA-Bio-R-Logo-Standard-White.png',
                            package='phenoptrReports')
    logo = png::readPNG(logo_path)

    add_logo_to_plot = function(p, logo) {
      # The logo aspect ratio is very close to 4:1. This puts it below
      # the scale bar with a size of 160 X 40.
      p +
        ggplot2::annotation_raster(grDevices::as.raster(logo),
                                   xlim[2]-50-160, xlim[2]-50,
                                   -(ylim[2]-65), -(ylim[2]-25))
    }
    p = add_logo_to_plot(p, logo)
  }

  # Get matching cells and add line segments to the plot as requested
  # Showing nearest neighbors requires two phenotypes
  if (have_pheno1 && have_pheno2) {
    if (show_as=='from_to') {
      # For each pheno1 cell, join with the data for the nearest pheno2 cell
      matching_cells = match_cells(pheno1_cells, pheno2_cells, pheno_name2)

      # Add lines
      p = p + ggplot2::geom_segment(data=matching_cells,
                          ggplot2::aes(xend=`Cell X Position.to`,
                                       yend=`Cell Y Position.to`),
                                       color='white') +
        ggplot2::labs(
          title=paste0(field_name, ' - Nearest ',
                       pheno_name2, ' to each ', pheno_name1))
    }
    else if (show_as=='to_from') {
      # for each pheno2 cell, find the nearest pheno1 cell
      matching_cells = match_cells(pheno2_cells, pheno1_cells, pheno_name1)

      p = p +
        ggplot2::geom_segment(data=matching_cells,
            ggplot2::aes(xend=`Cell X Position.to`,
                         yend=`Cell Y Position.to`),
                         color='white') +
        ggplot2::labs(
          title=paste0(field_name, ' - Nearest ',
                       pheno_name1, ' to each ', pheno_name2))
    }
    else if (show_as=='mutual') {
      # Mutual nearest neighbors
      # Mutual nearest neighbors are cells which have each other as nearest
      # neighbors; i.e. cells where the nearest neighbor of the nearest neighbor
      # is the starting cell.

      matching_cells = match_cells(pheno1_cells, pheno2_cells,
                                   pheno_name2, pheno_name1)
      match_col = id_column_name(pheno_name1) %>% rlang::sym()
      matching_cells = matching_cells %>%
        dplyr::filter(`Cell ID`==!!match_col)

      p = p +
        ggplot2::geom_segment(data=matching_cells,
            ggplot2::aes(xend=`Cell X Position.to`, yend=`Cell Y Position.to`),
                         size=1, color='white') +
        ggplot2::labs(title=paste0(field_name, ' - Mutual nearest neighbors - ',
                          pheno_name1, ' and ', pheno_name2))
    } else {
      # Don't show nearest neighbors, just cells
      matching_cells = NULL
      p = p +
        ggplot2::labs(title=paste0(field_name, ' - ',
                                   pheno_name1, ' and ', pheno_name2))
    }
  } else {
    # One or both phenotypes are missing, just show the field name as title
    matching_cells = NULL
    p = p + ggplot2::labs(title=field_name)
  }

  # Add circles for the cells
  # We want the circles on top of the lines, so add them last
  if (have_pheno1 && nrow(pheno1_cells) > 0)
    p = p + ggplot2::geom_point(data=pheno1_cells,
                                ggplot2::aes(color=pheno_name1), size=dot_size)
  if (have_pheno2 && nrow(pheno2_cells) > 0)
    p = p + ggplot2::geom_point(data=pheno2_cells,
                                ggplot2::aes(color=pheno_name2), size=dot_size)

  # Add the scale and logo again with transparency so the dots show through
  overlay_alpha = 0.7
  p = phenoptr:::add_scale_line(p, xlim, ylim,
                               scale_color='white', scale_alpha=overlay_alpha)

  if (add_logo) {
    logo[,,4] = logo[,,4] * overlay_alpha
    p = add_logo_to_plot(p, logo)
  }

  # A little theming
  p = p + ggplot2::theme(
            legend.key = ggplot2::element_rect(fill = "white"),
            legend.position='bottom') +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 5)))

  list(plot=p, data=matching_cells)
}

# Make sure field_data has distance columns for both phenotypes
# @param field_data Cell seg data for a (subset of) a single field
# @param phenos Named list of (two) phenotypes
# @return field_data with `Distance to <pheno>` and `Cell ID <pheno>`
# columns.
ensure_distance_columns = function(field_data, phenos) {
  pheno_names = unique(names(phenos))
  required_columns = c(distance_column_name(pheno_names),
                       id_column_name(pheno_names))

  if (!all(required_columns %in% names(field_data))) {
    # Remove any required fields that are already there so we don't duplicate
    field_data = field_data[, setdiff(names(field_data), required_columns)]
    field_data = dplyr::bind_cols(field_data,
                          phenoptr::find_nearest_distance(field_data, phenos))
  }
  field_data
}

# Join from_cells and to_cells by nearest neighbor cell ID
# and subset to the columns we want to export
# `from_name`, if provided, allows keeping the column needed for mutual matching
match_cells = function(from_cells, to_cells, to_name, from_name='.none.') {
  to_distance_col = distance_column_name(to_name)
  to_id_col = id_column_name(to_name) # Column with to_name IDs
  from_id_col = id_column_name(from_name)
  # Subset to interesting & necessary columns
  from_cells = from_cells %>%
    dplyr::select(
      dplyr::contains('Slide ID'),
      !!phenoptr::field_column(from_cells),
      `Cell ID`,
      dplyr::matches('Cell . Position'),
      dplyr::starts_with('Phenotype '),
      dplyr::contains('Tissue Category'),
      !!to_id_col,
      !!to_distance_col)

  to_cells = to_cells %>%
    dplyr::select(
      `Cell ID`,
      dplyr::matches('Cell . Position'),
      dplyr::starts_with('Phenotype '),
      dplyr::contains('Tissue Category'),
      dplyr::contains(from_id_col))

  by = rlang::set_names('Cell ID', to_id_col)
  matched = from_cells %>%
    dplyr::left_join(to_cells, by=by, suffix=c('', '.to'))

  # We don't really need 11 decimal places in the location & distance columns
  matched = matched %>%
    dplyr::mutate_at(.vars=dplyr::vars(dplyr::matches('Cell . Position|Distance to')),
                     ~round(., 2))
  matched
}

distance_column_name = function(pheno_names) {
  paste('Distance to', pheno_names)
}

id_column_name = function(pheno_names) {
  paste('Cell ID', pheno_names)
}

# Try to read a composite image for a field as a nativeRaster
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

  # Read the image as a nativeRaster
  if (file.exists(background_path))
  {
    if (grepl('jpg$', background_path))
      background = jpeg::readJPEG(background_path, native=TRUE)
    else background = tiff::readTIFF(background_path, native=TRUE)
  } else {
    background = NULL
  }
  background
}

# Get field metadata from the component data file
read_field_info = function(field, export_path) {
  field_base = stringr::str_remove(field, '\\.im3')
  component_path = file.path(export_path, paste0(field_base,
                                                 '_component_data.tif'))
  if(!file.exists(component_path)) {
    warning('File not found: "', component_path, '"')
    return(NULL)
  }

  field_info = phenoptr::get_field_info(component_path)
  field_info$pixels_per_micron = 1/field_info$microns_per_pixel
  field_info
}
