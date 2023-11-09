# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "Tissue Category Area (sq microns)",
  "Tissue Category Area (square microns)",
  "Tissue Area",
  "."
))

#' Compute cell densities from counts and tissue area
#'
#' Compute cell density from a table of cell counts and tissue areas
#' read from a summary cell seg data file.
#'
#' @param counts A data frame with columns for `.by`, `Tissue Category`,
#'   and counts, such as the output of [count_phenotypes].
#' @param summary_path Path(s) to cell seg data summary table(s) containing
#'   sample names and tissue categories matching `counts`.
#' @param tissue_categories A character vector of tissue category names
#'   of interest.
#' @param pixels_per_micron Conversion factor to microns.
#' @param .by Column to aggregate by
#' @return A data table with counts converted to density in  \eqn{cells / mm^2}.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
compute_density_from_cell_summary =
  function(counts, summary_path, tissue_categories,
           pixels_per_micron=getOption('phenoptr.pixels.per.micron'),
           .by='Slide ID') {

  stopifnot(rlang::as_string(.by) %in% names(counts))
  stopifnot('Tissue Category' %in% names(counts))

  .by = rlang::sym(.by)

  # Read the summary data
  summary_data = purrr::map_dfr(summary_path,
                                phenoptr::read_cell_seg_data,
                                pixels_per_micron=pixels_per_micron)

  # Drop rows for specific phenotypes, we just want one row per field
  if (any(startsWith(names(summary_data), 'Phenotype')))
    # This selects rows with 'All' in all phenotype columns
    summary_data = summary_data %>%
      dplyr::filter(if_all(dplyr::starts_with('Phenotype'), ~.x=='All'))

  # Manufacture Tissue Category Area if not present
  summary_data = ensure_tissue_category_area(summary_data)

  # Just the columns we need
  summary_data = summary_data %>%
    dplyr::select(!!.by, `Tissue Category`,
                  dplyr::starts_with('Tissue Category Area'))

  compute_density_from_table(counts, summary_data, tissue_categories, .by)
}

# Add Tissue Category and Tissue Category Area columns to summary
# data from inForm projects that don't segment tissue.
ensure_tissue_category_area = function(summary_data) {
  # These two columns should be written together but I have seen
  # data in the wild that has Tissue Category and not T C Area.

  # Add missing Tissue Category column
  if (!'Tissue Category' %in% names(summary_data))
    summary_data$`Tissue Category` = 'All'

  # Estimate missing area
  if (!any(startsWith(names(summary_data), 'Tissue Category Area'))) {
    summary_data = summary_data %>%
      dplyr::mutate(`Tissue Category Area (square microns)` =
                .data$`Total Cells` / .data$`Cell Density (per square mm)` * 1e6)
  }

  return (summary_data)
}

#' Compute cell densities from counts and tissue area
#'
#' Compute cell density from a table of cell counts and
#' a table of tissue areas.
#'
#' @param counts A data frame with columns for .by, `Tissue Category`,
#'   and counts, such as the output of [count_phenotypes].
#' @param areas A data frame containing tissue areas
#'   in \eqn{cells / micron^2} with
#'   sample names and tissue categories matching `counts`.
#' @param tissue_categories A character vector of tissue category names
#'   of interest.
#' @param .by Column to aggregate by
#' @return A data table with counts converted to density in \eqn{cells / mm^2}.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_density_from_table = function(counts, areas, tissue_categories,
                                      .by='Slide ID') {
  stopifnot(inherits(areas, 'data.frame'))
  .by = rlang::sym(.by)
  .by_str = rlang::as_string(.by)

  # Check that the files match
  missing_ids = setdiff(unique(counts[[.by_str]]),
                        unique(areas[[.by_str]]))

  if (length(missing_ids) > 0)
    stop(length(missing_ids),
         ' ', .by_str, 's missing from summary file (',
         paste(missing_ids, collapse=', '), ').')

  missing_categories = setdiff(tissue_categories,
                        unique(areas$`Tissue Category`))

  if (length(missing_categories) > 0)
    stop(length(missing_categories),
         ' tissue categories missing from summary file (',
         paste(missing_categories, collapse=', '), ').')

  # Normalize the name of the area column
  # Two different column names and two spellings of 'square'. Just brute force.
  if ("Region Area (sq microns)" %in% names(areas))
    areas = areas %>%
      dplyr::rename(`Tissue Area`="Region Area (sq microns)")
  else if ("Region Area (square microns)" %in% names(areas))
    areas = areas %>%
      dplyr::rename(`Tissue Area`="Region Area (square microns)")
  else if ('Tissue Category Area (sq microns)' %in% names(areas))
    areas = areas %>%
      dplyr::rename(`Tissue Area`='Tissue Category Area (sq microns)')
  else if ('Tissue Category Area (square microns)' %in% names(areas))
    areas = areas %>%
      dplyr::rename(`Tissue Area`='Tissue Category Area (square microns)')
  else stopifnot('Tissue Area' %in% names(areas))

  areas = areas %>%
    dplyr::filter(`Tissue Category` %in% tissue_categories) %>%
    dplyr::group_by(!!.by, `Tissue Category`) %>%
    dplyr::summarize(`Tissue Area` = sum(`Tissue Area`)/1e6) %>%
    add_tissue_category_totals(tissue_categories, .by)

  # If the inForm project has tissue categories, `counts` will
  # have 'Total' lines; change `areas`` to match. For projects without
  # tissue category, `counts` has a fake 'All' category so `areas`
  # doesn't need to change.
  if ('Total' %in% counts$`Tissue Category`)
    areas = areas %>%
      dplyr::mutate(`Tissue Category` =
                    dplyr::recode(`Tissue Category`, 'All'='Total'))

  # Join with the counts table and divide counts by area to get density
  densities = dplyr::left_join(counts, areas) %>%
    dplyr::select(!!.by, `Tissue Category`, `Tissue Area`,
                  dplyr::everything()) %>%
    dplyr::mutate_at(-(1:3), function(x) x/.$`Tissue Area`) %>%
    dplyr::rename(`Tissue Area (mm2)`=`Tissue Area`)

  densities
}
