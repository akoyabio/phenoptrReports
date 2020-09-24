#' Process ROIs for the cells in csd
#' @param csd Consolidated cell seg table
#' @param study_dir Directory containing annotation files for the source
#' images in `csd`
#' @param require_include Should the result
#' include only cells contained in ROIs tagged with `#IncludeInResults`?
#' @return A list containing three items:
#' - `csd`: `csd` filtered by `#ExcludeFromResults` and (optionally)
#'   `#IncludeInResults` ROIs, and with extra columns added to show
#'   membership in any other tagged ROIs.
#' - `removed`: A data frame with counts of cells removed.
#' - `stats`: A data frame with counts of cells in other tags.
#' @keywords internal
process_rois = function(csd, study_dir, require_include) {
  if (is.null(study_dir) || !dir.exists(study_dir))
    stop('Missing or invalid study directory: ', study_dir)

  if (!'Sample Name' %in% names(csd) ||
      !all(endsWith(csd$`Sample Name`, '.qptiff')))
    stop('process_rois is only valid for analyses with QPTIFF images.')

  samples = unique(csd$`Sample Name`) %>%
    stringr::str_remove('\\.qptiff$')
  annotation_paths = list.files(study_dir, pattern='_annotations.xml$',
                                full.names=TRUE, recursive=TRUE)

  # Warn about any sample without annotations (should this be an error?)
  annotation_images = purrr::map_chr(annotation_paths, basename) %>%
    stringr::str_remove('_annotations.xml$')

  missing_annotations = setdiff(samples, annotation_images)
  if (length(missing_annotations) > 0)
    warning('Annotation files are missing for image(s): ',
            paste(missing_annotations, collapse=', '))

  # Process by image
  csd_nested = csd %>%
    dplyr::group_by(`Sample Name`) %>%
    tidyr::nest()

  removed = NULL
  stats = NULL

  # Replace csd_nested$data with filtered and expanded data
  # while accumulating `removed` and `stats`
  csd_nested$data = purrr::pmap(csd_nested, function(`Sample Name`, data) {
    sample_results =
      process_rois_single(`Sample Name`, data, annotation_paths, require_include)
    removed <<- dplyr::bind_rows(removed, sample_results$removed)
    stats <<- dplyr::bind_rows(stats, sample_results$stats)
    sample_results$csd
  })

  # Finish up
  removed = tibble::add_column(removed, `Sample Name`=csd_nested$`Sample Name`,
                               .before=1)

  stats = tibble::add_column(stats, `Sample Name`=csd_nested$`Sample Name`,
                             .before=1)
  stats[is.na(stats)] = 0

  csd = tidyr::unnest(csd_nested, 'data') %>% dplyr::ungroup()

  return(list(csd=csd, removed=removed, stats=stats))
}

#' Process ROIs for a single image
#' @param sample_name Name of the sample image (without the extension)
#' @data Cell seg data for the sample image (only)
#' @param annotation_paths Full paths to all candidate annotation files
#' @param require_include Should the result
#' include only cells contained in ROIs tagged with `#IncludeInResults`?
#' @return A list containing three items:
#' - `csd`: `data` filtered by `#ExcludeFromResults` and (optionally)
#'   `#IncludeInResults` ROIs, and with extra columns added to show
#'   membership in any other tagged ROIs.
#' - `removed`: A data frame with counts of cells removed.
#' - `stats`: A data frame with counts of cells in other tags.
#' @keywords internal
process_rois_single = function(
    sample_name, data, annotation_paths, require_include) {
  # Find the annotation file
  sample = stringr::str_remove(sample_name, '\\.qptiff$')
  annotation_file = stringr::str_subset(annotation_paths, sample)
  if (length(annotation_file) > 1)
    stop('Multiple annotation files found for ', sample)

  # Get named ROIs from the annotation file
  rois = phenoptr::read_phenochart_polygons(annotation_file)
  if (nrow(rois) > 0) {
    rois = rois %>% filter(tags != '')
    all_roi_names = rois$tags %>%
      stringr::str_split(' ') %>%
      unlist() %>%
      unique() %>%
      sort()
  } else all_roi_names = character()

  # Hard-coded tag names with special meaning
  include_name = '#IncludeInResults'
  exclude_name = '#ExcludeFromResults'
  special_roi_names =
    if (require_include) c(include_name, exclude_name) else exclude_name
  generic_roi_names = setdiff(all_roi_names, special_roi_names)

  # Places to accumulate cell counts
  removed = rep(0, length(special_roi_names)) %>%
    setNames(special_roi_names) %>%
    tibble::as_tibble_row()

  stats = rep(0, length(generic_roi_names)) %>%
    setNames(generic_roi_names) %>%
    tibble::as_tibble_row()

  if (require_include && !include_name %in% all_roi_names) {
    # There is no include ROI, everything is discarded
    removed[include_name] = nrow(data)
    return(list(csd=data[0,], removed=removed, stats=stats))
  }

  # Start spatial processing by adding a geometry column
  data = phenoptr::add_geometry(data)

  # Process include ROIs if requested
  if (require_include) {
    # At this point we know we have at least one include ROI
    include_roi = rois %>%
      dplyr::filter(stringr::str_detect(tags, include_name)) %>%
      sf::st_union()
    before_count = nrow(data)
    # Note: st_intersects with x=include_roi is faster than
    # st_intersection with x=data
    intersects =
      sf::st_intersects(include_roi, data, sparse=FALSE)[1, , drop=TRUE]
    data = data[intersects, ]
    removed[include_name] = before_count - nrow(data)
  }

  # Process exclude ROIs if any
  if (exclude_name %in% all_roi_names) {
    exclude_roi = rois %>%
      dplyr::filter(stringr::str_detect(tags, exclude_name)) %>%
      sf::st_union()
    before_count = nrow(data)
    intersects =
      sf::st_intersects(exclude_roi, data, sparse=FALSE)[1, , drop=TRUE]
    data = data[!intersects, ]
    removed[exclude_name] = before_count - nrow(data)
  }

  # For each remaining (generic) ROI:
  # - Add a boolean column to data with the name of the tag,
  #   reflecting membership in the tag.
  # - Add a cell count to `stats`
  for (roi_name in generic_roi_names) {
    generic_roi = rois %>%
      dplyr::filter(stringr::str_detect(tags, roi_name)) %>%
      sf::st_union()
    membership =
      sf::st_intersects(generic_roi, data, sparse=FALSE)[1, , drop=TRUE]
    data[roi_name] = membership
    stats[roi_name] = sum(membership)
  }

  # Remove the geometry from data
  data = sf::st_drop_geometry(data)

  return(list(csd=data, removed=removed, stats=stats))
}
