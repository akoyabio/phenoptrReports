# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "tags"
))

#' Process ROIs for the cells in csd and associated tissue category areas.
#'
#' `csd` will be filtered according to ROIs in the associated images'
#' annotation files.
#'
#' Updated summary data files with corrected tissue category areas
#' will be written to `output_dir`.
#'
#' @param csd Consolidated cell seg table
#' @param study_dir Directory containing annotation files for the source
#' images in `csd`
#' @param export_dir A directory containing `binary_seg_maps` files and
#' merged cell seg data summary files for processing.
#' @param output_dir Directory where new files will be written.
#' @param require_include Should the result
#' include only cells contained in ROIs tagged with `#IncludeInResults`?
#' @return A list containing three items:
#' - `csd`: `csd` filtered by `#ExcludeFromResults` and (optionally)
#'   `#IncludeInResults` ROIs, and with extra columns added to show
#'   membership in any other tagged ROIs.
#' - `removed`: A data frame with counts of cells removed.
#' - `stats`: A data frame with counts of cells in other tags.
#' @keywords internal
process_rois = function(csd, study_dir, export_dir, output_dir,
                        require_include) {
  # Parameter checks
  if (is.null(study_dir) || !dir.exists(study_dir))
    stop('Missing or invalid study directory: ', study_dir)

  if (is.null(export_dir) || !dir.exists(export_dir))
    stop('Missing or invalid export directory: ', export_dir)

  if (is.null(output_dir) || !dir.exists(output_dir))
    stop('Missing or invalid output directory: ', output_dir)

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
  areas = NULL

  # Replace csd_nested$data with filtered and expanded data
  # while accumulating `removed`, `stats` and `areas`.
  csd_nested$data = purrr::pmap(csd_nested, function(`Sample Name`, data) {
    # Get ROIs and process cell data
    sample_results =
      process_rois_single(`Sample Name`, data, annotation_paths,
                          require_include, output_dir)

    # Update stats
    removed <<- dplyr::bind_rows(removed, sample_results$removed)
    stats <<- dplyr::bind_rows(stats, sample_results$stats)

    # Process tissue category data and update areas
    # Note we only have to process annotations that are still in the data
    annotations = unique(sample_results$csd$`Annotation ID`)
    areas <<- dplyr::bind_rows(areas,
                process_tissue_categories_single(
                  `Sample Name`, annotations,
                  sample_results$include_roi, sample_results$exclude_roi,
                  export_dir, output_dir))

    sample_results$csd
  })

  # Now create new summary data files with the corrected tissue area
  update_summary_data(export_dir, output_dir, areas)

  # Finish up
  removed = tibble::add_column(removed, `Sample Name`=csd_nested$`Sample Name`,
                               .before=1)

  stats = tibble::add_column(stats, `Sample Name`=csd_nested$`Sample Name`,
                             .before=1)
  stats[is.na(stats)] = 0

  csd = tidyr::unnest(csd_nested, 'data') %>% dplyr::ungroup()

  return(list(csd=csd, removed=removed, stats=stats))
}

#' Process ROIs for a single image, removing cells that don't meet
#' the inclusion and exclusion criteria.
#'
#' @param sample_name Name of the sample image (without the extension)
#' @param data Cell seg data for the sample image (only)
#' @param annotation_paths Full paths to all candidate annotation files
#' @param require_include Should the result
#' include only cells contained in ROIs tagged with `#IncludeInResults`?
#' @param output_dir Directory to save a check plot
#' @return A list containing these items:
#' - `csd`: `data` filtered by `#ExcludeFromResults` and (optionally)
#'   `#IncludeInResults` ROIs, and with extra columns added to show
#'   membership in any other tagged ROIs.
#' - `removed`: A data frame with counts of cells removed.
#' - `stats`: A data frame with counts of cells in other tags.
#' - `include_roi`: The union of all #Include ROIs, or NULL if none.
#' - `exclude_roi`: The union of all #Exclude ROIs, or NULL if none.
#' @keywords internal
process_rois_single = function(
    sample_name, data, annotation_paths, require_include, output_dir) {
  # Find the annotation file
  sample = stringr::str_remove(sample_name, '\\.qptiff$')
  annotation_file = stringr::str_subset(annotation_paths, sample)
  if (length(annotation_file) > 1)
    stop('Multiple annotation files found for ', sample)

  cat('Processing ROIs for', sample, '\n')

  # Get named ROIs from the annotation file
  rois = phenoptr::read_phenochart_polygons(annotation_file)
  if (nrow(rois) > 0) {
    rois = rois %>% dplyr::filter(tags != '')
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
    rlang::set_names(special_roi_names) %>%
    tibble::as_tibble_row()

  stats = rep(0, length(generic_roi_names)) %>%
    rlang::set_names(generic_roi_names) %>%
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
    cat('Trimming cells not in #Include ROIs\n')
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
  } else {
    include_roi = NULL
  }

  # Process exclude ROIs if any
  if (exclude_name %in% all_roi_names) {
    cat('Trimming cells in #Exclude ROIs\n')
    exclude_roi = rois %>%
      dplyr::filter(stringr::str_detect(tags, exclude_name)) %>%
      sf::st_union()

    # Filter cells
    before_count = nrow(data)
    intersects =
      sf::st_intersects(exclude_roi, data, sparse=FALSE)[1, , drop=TRUE]
    data = data[!intersects, ]
    removed[exclude_name] = before_count - nrow(data)
  } else {
    exclude_roi = NULL
  }

  # For each remaining (generic) ROI:
  # - Add a boolean column to data with the name of the tag,
  #   reflecting membership in the tag.
  # - Add a cell count to `stats`
  for (roi_name in generic_roi_names) {
    cat('Tagging cells in ', roi_name, '\n')
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

  # Save a check plot
  cell_plot_path = file.path(output_dir,
                             paste0(sample, '_trimmed_cells.png'))
  cat('Saving cell check plot to ', cell_plot_path, '\n')

  # Make a phenotype colummn
  phenos = data %>%
    select(starts_with('Phenotype')) %>%
    do.call(paste, .) %>%
    stringr::str_trim()

  grDevices::png(cell_plot_path, type='cairo', antialias='gray',
                 width=980, height=980)
  p = data %>%
    dplyr::mutate(Phenotype=phenos) %>%
    dplyr::filter(stringr::str_detect(Phenotype, '\\+')) %>%
    ggplot2::ggplot(ggplot2::aes(`Cell X Position`, `Cell Y Position`,
                                 color=Phenotype)) +
    ggplot2::geom_point(shape='.', alpha=0.8) +
    ggplot2::coord_equal() +
    ggplot2::scale_y_reverse() +
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes=list(shape=19, size=3, alpha=1), ncol=1)) +
    ggplot2::labs(title=paste(sample, 'trimmed cells')) +
    ggplot2::theme_minimal()

  print(p)
  grDevices::dev.off()


  return(list(csd=data, removed=removed, stats=stats,
              include_roi=include_roi, exclude_roi=exclude_roi))
}

#' Get corrected tissue category areas for a single image.
#' @param sample_name Name of the image being processed
#' @param annotations Annotation IDs of interest
#' @param include_roi ROI of include area, or `NULL` to include all
#' @param exclude_roi ROI of exclude area, or `NULL` if none
#' @param export_dir Path to directory containing `binary_seg_maps` files.
#' @param output_dir Directory to write a check plot.
#' @return A data frame with columns for annotation ID, tissue category
#' and tissue category area.
#' @keywords internal
process_tissue_categories_single = function(
  sample_name, annotations, include_roi, exclude_roi, export_dir, output_dir) {
  # Consolidate the ROIs to a single trim_roi
  if (is.null(include_roi))
    trim_roi = find_bounding_box(annotations, export_dir)
  else trim_roi = include_roi

  if (!is.null(exclude_roi))
    trim_roi = sf::st_difference(trim_roi, exclude_roi)

  # Get the updated areas and save a reference plot
  sample = stringr::str_remove(sample_name, '\\.qptiff$')
  trim_plot_path = file.path(output_dir,
                             paste0(sample, '_trimmed_categories.png'))
  result = phenoptr::trim_tissue_categories(annotations, trim_roi,
                                   export_dir, trim_plot_path)
  result
}

#' Get a bounding box for the given annotations as a polygon ROI
#' @param annotations A list of Annotation IDs of interest
#' @param export_dir inForm export directory containing segmentation map
#' files for the annotations of interest
#' @return A rectangular polygon which encloses all the given annotations.
#' @keywords internal
find_bounding_box = function(annotations, export_dir) {
  # Find annotations at the top, left, bottom and right of the range
  centers = stringr::str_match(annotations, '\\[(\\d+),(\\d+)\\]')[, 2:3]
  colnames(centers) = c('x', 'y')
  centers = centers %>%
    tibble::as_tibble(.name_repair='universal') %>%
    dplyr::mutate_all(as.numeric)

  # Get dimensions of an annotation from the binary seg map file
  annotation_dims = function(annotation) {
    map_path = phenoptr::get_map_path(annotation, export_dir)
    tiff::readTIFFDirectory(map_path)
  }

  # Position is in microns so directly usable
  # Resolution is in pixels per cm so convert to pixels per micron
  left = annotation_dims(annotations[which.min(centers$x)])$x.position
  right = annotation_dims(annotations[which.max(centers$x)])
  right = right$x.position + right$width * 10000 / right$x.resolution

  top  = annotation_dims(annotations[which.min(centers$y)])$y.position
  bottom  = annotation_dims(annotations[which.max(centers$y)])
  bottom = bottom$y.position + bottom$length * 10000 / bottom$y.resolution

  sf::st_bbox(c(xmin=left, xmax=right, ymin=top, ymax=bottom)) %>%
    sf::st_as_sfc()
}

#' Find merged cell seg summary files in `export_dir`. Make corrected ones by
#' substituting tissue category areas in the files with the ones in `areas`.
#'
#' Write new files with corrected areas and selected columns to `output_dir`.
#' @param export_dir An inForm export directory containing merged cell seg
#' data files.
#' @param output_dir Directory to receive corrected files.
#' @param areas A data frame with columns for Annotation ID, Tissue Category
#' and category area.
#' @keywords internal
update_summary_data = function(export_dir, output_dir, areas) {
  candidates =
    list.files(export_dir, '_cell_seg_data_summary.txt$', full.names=TRUE)

  # We don't know what name was used for the merge (it doesn't have to be
  # 'Merge') so weed out obvious non-merge files and use what is left.
  candidates = candidates %>%
    purrr::discard(~stringr::str_detect(.x, 'reject')) %>%
    purrr::discard(~stringr::str_detect(.x, '\\[\\d+,\\d+\\]'))

  # Process a single candidate
  process_one = function(candidate) {
    df = phenoptr::read_cell_seg_data(candidate)

    # Missing columns means we picked an incorrect file?
    required_names = c('Annotation ID', 'Tissue Category', 'Phenotype',
                       "Tissue Category Area (square microns)")
    if (!all(required_names %in% names(df))) {
      cat('Skipping ', basename(candidate), '\n')
      return()
    }

    # Unknown tumor categories means something is really wrong
    missing_cats = setdiff(unique(df$`Tissue Category`), areas$`Tissue Category`)
    if (length(missing_cats) > 0)
      stop(basename(candidate), ' contains unknown categories: ',
           paste(missing_cats, collapse=', '))

    # Replace candidate areas with new ones
    # Inner join will drop annotations that don't appear in areas.
    # We want this - they have been excluded from the analysis.
    df = df %>%
      dplyr::select(dplyr::contains('Path'),
                    dplyr::contains('Slide ID'),
                    dplyr::contains('Sample Name'),
                    'Annotation ID', 'Tissue Category', 'Phenotype') %>%
      dplyr::filter(Phenotype=='All') %>%
      dplyr::inner_join(areas, by=c('Annotation ID', 'Tissue Category'))

    # Write the updated file
    trimmed_path =
      file.path(output_dir, paste0('Trimmed_', basename(candidate)))
    cat('Writing', basename(trimmed_path), '\n')
    readr::write_tsv(df, trimmed_path, na='#N/A')
  }

  purrr::walk(candidates, process_one)
}

#' Write a workbook with stats from the ROI trimming
#' @param roi_results Results from [process_rois]
#' @param output_dir Directory for the report
#' @keywords internal
write_roi_stats <- function(roi_results, output_dir) {
  wb = openxlsx::createWorkbook()
  write_sheet(wb, roi_results$removed, 'Cells removed',
              "Cells removed by ROI",
              header_col=2, addGrid=FALSE)
  openxlsx::setColWidths(wb, 'Cells removed',
                         cols=2:ncol(roi_results$removed), widths=20)

  if (ncol(roi_results$stats) > 1) {
    write_sheet(wb, roi_results$stats, 'Cells in ROIs',
                "Cells in other ROIs",
                header_col=2, addGrid=FALSE)
    openxlsx::setColWidths(wb, 'Cells in ROIs',
                           cols=2:ncol(roi_results$stats), widths=20)
  }

  workbook_path = file.path(output_dir, "ROI_Results.xlsx")
  if (file.exists(workbook_path)) file.remove(workbook_path)
  openxlsx::saveWorkbook(wb, workbook_path)
}
