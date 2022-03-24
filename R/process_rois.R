# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "tags", "Annotation ID"
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
#' @param export_dir Optional directory containing `binary_seg_maps` files and
#' merged cell seg data summary files for processing.
#' @param output_dir Directory where new files will be written.
#' @param require_include Should the result
#' include only cells contained in ROIs tagged with `#IncludeInResults`?
#' @param extra_include Optional vector of additional tags to treat as include
#' tags.
#' @param extra_exclude Option vector of additional tags to treat as
#' exclude tags.
#' @return A list containing four items:
#' - `csd`: `csd` filtered by `#ExcludeFromResults` and (optionally)
#'   `#IncludeInResults` ROIs, and with extra columns added to show
#'   membership in any other tagged ROIs.
#' - `summary`: A data frame summarizing tissue area and cell counts
#' in the filtered data.
#' - `removed`: A data frame with counts of cells removed.
#' - `stats`: A data frame with counts of cells in other tags.
#' @keywords internal
process_rois = function(csd, study_dir, export_dir, output_dir,
                        require_include,
                        extra_include=NULL, extra_exclude=NULL) {
  # Parameter checks
  if (is.null(study_dir) || !dir.exists(study_dir))
    stop('Missing or invalid study directory: ', study_dir)

  if (is.null(output_dir) || !dir.exists(output_dir))
    stop('Missing or invalid output directory: ', output_dir)

  if (!'Sample Name' %in% names(csd) ||
      !all(endsWith(csd$`Sample Name`, '.qptiff')))
    stop('process_rois is only valid for analyses with QPTIFF images.')

  samples = dplyr::distinct(csd, `Sample Name`) %>%
    dplyr::pull() %>%
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
                          require_include, output_dir,
                          extra_include, extra_exclude)

    # Update stats
    removed <<- dplyr::bind_rows(removed, sample_results$removed)
    stats <<- dplyr::bind_rows(stats, sample_results$stats)

    # Process tissue category data and update areas
    # Note we only have to process annotations that are still in the data
    # Only if the export directory was provided
    if (!is.null(export_dir)) {
      # Only if we have any required #Include ROI
      if (!require_include || !is.null(sample_results$include_roi)) {
        annotations = unique(sample_results$csd$`Annotation ID`)
        areas <<- dplyr::bind_rows(areas,
                    process_tissue_categories_single(
                      `Sample Name`, annotations,
                      sample_results$include_roi, sample_results$exclude_roi,
                      export_dir, output_dir))
      }
    }

    sample_results$csd
  })

  # Create new summary data files with the corrected tissue area
  # This creates the original "trimmed" cell seg summary containing just
  # tissue areas for a single export
  if (!is.null(export_dir))
    update_summary_data(export_dir, output_dir, areas)

  csd = tidyr::unnest(csd_nested, 'data') %>% dplyr::ungroup()

  # This creates a more comprehensive summary which includes cell counts
  # as well as tissue area
  summ = summarize_cell_seg_data(csd, areas)

  # Finish up
  removed = tibble::add_column(removed, `Sample Name`=csd_nested$`Sample Name`,
                               .before=1)

  stats = tibble::add_column(stats, `Sample Name`=csd_nested$`Sample Name`,
                             .before=1)
  stats[is.na(stats)] = 0

  return(list(csd=csd, summary=summ, removed=removed, stats=stats))
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
#' @param extra_include Optional vector of additional tags to treat as include
#' tags.
#' @param extra_exclude Option vector of additional tags to treat as
#' exclude tags.
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
    sample_name, data, annotation_paths, require_include, output_dir,
    extra_include=NULL, extra_exclude=NULL) {
  # Find the annotation file
  sample = stringr::str_remove(sample_name, '\\.qptiff$')
  sample_regex = paste0('\\Q', sample, '\\E')
  annotation_file = stringr::str_subset(annotation_paths, sample_regex)

  if (length(annotation_file) == 0)
    stop('No annotation file found for ', sample)
  if (length(annotation_file) > 1)
    stop('Multiple annotation files found for ', sample)

  cat('Processing ROIs for', sample, '\n')

  # Get named ROIs from the annotation file
  # If a GeoJSON file is available, prefer that
  geojson_path = stringr::str_replace(annotation_file, '\\.xml$', '.geojson')
  if (file.exists(geojson_path)) {
    annotation_file = geojson_path
    message('Reading annotations from ', annotation_file)
  } else {
    warning('Reading annotations from ', annotation_file,
            '\nDeleted, tagged annotations may be included.')
  }

  rois = phenoptr::read_tagged_rois(annotation_file)
  all_roi_names = names(rois)

  # Hard-coded tag names with special meaning
  include_name = '#IncludeInResults'
  exclude_name = '#ExcludeFromResults'
  include_roi_names =
    if (require_include) c(include_name, extra_include) else extra_include
  exclude_roi_names =
    if (exclude_name %in% all_roi_names)
      c(exclude_name, extra_exclude) else extra_exclude

  special_roi_names = c(include_roi_names, exclude_roi_names)
  generic_roi_names = setdiff(all_roi_names, special_roi_names)

  # Build single include and exclude ROIs and check for missing ones
  include_roi = build_roi(rois, include_roi_names)
  exclude_roi = build_roi(rois, exclude_roi_names)

  # Places to accumulate cell counts
  removed = rep(0, length(special_roi_names)) %>%
    rlang::set_names(special_roi_names) %>%
    tibble::as_tibble_row()

  stats = rep(0, length(generic_roi_names)) %>%
    rlang::set_names(generic_roi_names) %>%
    tibble::as_tibble_row()

  # Start spatial processing by adding a geometry column
  data = phenoptr::add_geometry(data)

  # Process include ROIs if requested
  if (!is.null(include_roi)) {
    # At this point we know we have an include ROI
    cat('Trimming cells not in',
        paste(include_roi_names, collapse=', '), '\n')
    before_count = nrow(data)
    # Note: st_intersects with x=include_roi is faster than
    # st_intersection with x=data
    intersects =
      sf::st_intersects(include_roi, data, sparse=FALSE)[1, , drop=TRUE]
    data = data[intersects, ]
    removed[include_name] = before_count - nrow(data)

    # Trim the exclude region to the include region
    if (!is.null(exclude_roi)) {
      exclude_roi = sf::st_intersection(include_roi, exclude_roi)

      # If the intersection is empty, exclude_roi becomes an empty list
      # Change it to NULL for consistency
      if (length(exclude_roi) == 0) exclude_roi = NULL
    }
  }

  # Process exclude ROIs if any
  if (!is.null(exclude_roi)) {
    cat('Trimming cells in',
        paste(exclude_roi_names, collapse=', '), '\n')

    # Filter cells
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
    cat('Tagging cells in', roi_name, '\n')
    generic_roi = rois[[roi_name]]
    membership =
      sf::st_intersects(generic_roi, data, sparse=FALSE)[1, , drop=TRUE]
    data[roi_name] = membership
    stats[roi_name] = sum(membership)
  }

  # Remove the geometry from data
  data = sf::st_drop_geometry(data)

  # Save a check plot colored by phenotype
  cell_plot_path = file.path(output_dir,
                             paste0(sample, '_trimmed_cells.png'))
  cat('Saving cell check plot to ', cell_plot_path, '\n')

  # Make a phenotype column
  phenos = data %>%
    dplyr::select(dplyr::starts_with('Phenotype')) %>%
    do.call(paste, .) %>%
    stringr::str_trim()

  grDevices::png(cell_plot_path, type='cairo', antialias='gray',
                 width=980, height=980)

  # Helper to add ROIs
  add_rois_to_plot = function(p) {
    # suppressMessages prevents message about replacing the plot's coord
    if (!is.null(include_roi))
      p = suppressMessages(p + phenoptr::geom_sf_invert(data=include_roi,
                                  inherit.aes=FALSE, fill=NA))
    if (!is.null(exclude_roi))
      p = suppressMessages(p + phenoptr::geom_sf_invert(data=exclude_roi,
                                  inherit.aes=FALSE,
                                  color='grey50', fill='#AAAAAA33'))
    p
  }

  # Create a plot with funny coordinates so we can add the ROIs
  # with geom_sf_invert and have it all work out
  p = data %>%
    dplyr::mutate(Phenotype=phenos) %>%
    dplyr::filter(stringr::str_detect(Phenotype, '\\+')) %>%
    ggplot2::ggplot(ggplot2::aes(`Cell X Position`, -`Cell Y Position`,
                                 color=Phenotype)) +
    ggplot2::geom_point(shape='.', alpha=0.8) +
    ggplot2::coord_sf() +
    phenoptr::scale_sf_invert() +
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes=list(shape=19, size=3, alpha=1), ncol=1)) +
    ggplot2::labs(title=paste(sample, 'trimmed cells'),
                  y='Cell Y Position') +
    ggplot2::theme_minimal()

  p = add_rois_to_plot(p)
  print(p)
  grDevices::dev.off()

  # Another check plot colored by tissue category
  if ('Tissue Category' %in% names(data)) {
    cell_plot_path = file.path(output_dir,
                               paste0(sample, '_trimmed_cells_by_tissue.png'))
    cat('Saving cell by tissue check plot to ', cell_plot_path, '\n')

    grDevices::png(cell_plot_path, type='cairo', antialias='gray',
                   width=980, height=980)
    p = data %>%
      ggplot2::ggplot(ggplot2::aes(`Cell X Position`, -`Cell Y Position`,
                                   color=`Tissue Category`)) +
      ggplot2::geom_point(shape='.', alpha=0.8) +
      ggplot2::coord_sf() +
      phenoptr::scale_sf_invert() +
      ggplot2::guides(color = ggplot2::guide_legend(
        override.aes=list(shape=19, size=3, alpha=1), ncol=1)) +
      ggplot2::labs(title=paste(sample, 'trimmed cells by tissue category'),
                    y='Cell Y Position') +
      ggplot2::theme_minimal()

    p = add_rois_to_plot(p)
    print(p)
    grDevices::dev.off()
  }

  return(list(csd=data, removed=removed, stats=stats,
              include_roi=include_roi, exclude_roi=exclude_roi))
}

#' Build ROIs from multiple pieces
#' @param rois A named list of all candidate ROI pieces
#' @param roi_names Names of the ROIs to include in the result
#' @return A single `sf::sfc` object containing the ROI of interest, or `NULL`
#' if `roi_names` is `NULL`.
#' @keywords internal
build_roi = function(rois, roi_names) {
  if (is.null(roi_names) || length(roi_names)==0)
    return(NULL) # Nothing to do

  # Check for missing ROIs
  missing = setdiff(roi_names, names(rois))
  if (length(missing) > 0)
    stop('Missing ROI(s): ', paste(missing, collapse=', '))

  rois[names(rois) %in% roi_names] %>%
    purrr::reduce(sf::st_union)
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
  if (is.null(export_dir))
    stop('Export directory required in process_tissue_categories_single.')

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
#' @return A rectangular polygon, with dimensions in microns,
#' which encloses all the given annotations.
#' @keywords internal
find_bounding_box = function(annotations, export_dir) {
  # Find annotations at the top, left, bottom and right of the range
  centers = stringr::str_match(annotations, '\\[(\\d+),(\\d+)\\]')[, 2:3, drop=FALSE]
  colnames(centers) = c('x', 'y')
  centers = centers %>%
    tibble::as_tibble(.name_repair='universal') %>%
    dplyr::mutate_all(as.numeric)

  # Get dimensions of an annotation from the binary seg map file
  annotation_dims = function(annotation) {
    map_path = phenoptr::get_map_path(annotation, export_dir)
    phenoptr::readTIFFDirectory(map_path)
  }

  # Position is in cm, resolution is in pixels per cm
  # First compute all in cm
  left = annotation_dims(annotations[which.min(centers$x)])$x.position
  right = annotation_dims(annotations[which.max(centers$x)])
  right = right$x.position + right$width / right$x.resolution

  top  = annotation_dims(annotations[which.min(centers$y)])$y.position
  bottom  = annotation_dims(annotations[which.max(centers$y)])
  bottom = bottom$y.position + bottom$length / bottom$y.resolution

  mu_per_cm = 10000
  bbox = sf::st_bbox(c(xmin=left, xmax=right, ymin=top, ymax=bottom)) %>%
    sf::st_as_sfc()
  bbox * mu_per_cm
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
    list.files(export_dir, '_cell_seg_data_summary.txt$',
               recursive=TRUE, full.names=TRUE)

  # We don't know what name was used for the merge (it doesn't have to be
  # 'Merge') so weed out obvious non-merge files and use what is left.
  candidates = candidates %>%
    purrr::discard(~stringr::str_detect(.x, 'reject')) %>%
    purrr::discard(~stringr::str_detect(.x, '\\[\\d+,\\d+\\]'))

  if (length(candidates) == 0)
    warning('No Merge_cell_seg_data_summary.txt to update in ', export_dir)

  # Process a single candidate
  process_one = function(candidate) {
    df = phenoptr::read_cell_seg_data(candidate)

    # Missing columns means we picked an incorrect file?
    required_names = c('Annotation ID', 'Tissue Category',
                       "Tissue Category Area (square microns)")
    missing_names = setdiff(required_names, names(df))

    # Look for 'Phenotype' or 'Phenotype-xx'
    if (!any(stringr::str_detect(names(df), '^Phenotype')))
      missing_names = c(missing_names, 'Phenotype')
    if (length(missing_names) > 0) {
      cat('Skipping ', basename(candidate), '\n',
          'Missing required columns: ',
          paste(missing_names, collapse=', '), '\n')
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
                    'Annotation ID', 'Tissue Category',
                    dplyr::starts_with('Phenotype')) %>%
      # This selects rows with 'All' in all phenotype columns
      dplyr::filter(
        dplyr::if_all(dplyr::starts_with('Phenotype'), ~.x=='All')) %>%
      dplyr::inner_join(areas, by=c('Annotation ID', 'Tissue Category'))

    # Write the updated file
    trimmed_path =
      file.path(output_dir, paste0('Trimmed_', basename(candidate)))
    cat('Writing', basename(trimmed_path), '\n')
    readr::write_tsv(df, trimmed_path, na='#N/A')
  }

  purrr::walk(candidates, process_one)
}

#' Create a summary data file from consolidated cell seg data and, optionally,
#' tissue areas.
#' @param csd Consolidated cell seg data to summarize
#' @param areas Data frame of tissue area by annotation and category
#' @return A data frame with columns for Slide ID, Annotation ID,
#' Tissue Category, cell counts for each positive phenotype, and tissue area
#' @keywords internal
summarize_cell_seg_data = function(csd, areas=NULL) {
  tissue_cats = sort(unique(csd$`Tissue Category`))

  # Get phenotype columns and make fill list for `tidyr::complete`
  pheno_cols = names(csd) %>% stringr::str_subset('^Phenotype')
  pheno_fill = rep(0, length(pheno_cols)) %>%
    rlang::set_names(pheno_cols) %>%
    c(All=0) %>%
    as.list()

  summ = csd %>%
    # Count phenotypes by Annotation ID and Tissue Category
    dplyr::select(`Annotation ID`, `Tissue Category`, !!!pheno_cols) %>%
    dplyr::group_by(`Annotation ID`, `Tissue Category`) %>%
    dplyr::summarize(
      All=dplyr::n(), # Total number of cells
      dplyr::across(dplyr::starts_with('Phenotype'), ~sum(endsWith(.x, '+'))),
      .groups='drop') %>%

    # Fill in any missing Tissue Category
    tidyr::complete(`Annotation ID`, `Tissue Category`, fill=pheno_fill) %>%

    # Add summary row per Annotation ID
    add_tissue_category_totals(tissue_cats, .by='Annotation ID',
                               total_name='All')

  # Merge with areas, if provided
  if (!is.null(areas)) {
    summ = dplyr::full_join(summ, areas) %>%
      tidyr::replace_na(pheno_fill)
  }

  # Add back the Slide ID, if available
  if ('Slide ID' %in% names(csd)) {
    slides = csd %>%
      dplyr::select(`Slide ID`, `Annotation ID`) %>%
      dplyr::distinct()

    summ = dplyr::right_join(slides, summ)
  }

  summ
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
