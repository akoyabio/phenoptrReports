# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "Tissue Category Area (sq microns)",
  "Tissue Area",
  "."
))

#' Compute cell densities from counts and tissue area
#'
#' Compute cell density from a table of cell counts and tissue areas
#' read from a summary cell seg data file.
#'
#' @param counts A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and counts, such as the output of [count_phenotypes].
#' @param summary_path Path(s) to cell seg data summary table(s) containing
#'   sample names and tissue categories matching `counts`.
#' @param tissue_categories A character vector of tissue category names
#'   of interest.
#' @param pixels_per_micron Conversion factor to microns.
#' @return A data table with counts converted to density in  \eqn{cells / mm^2}.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_density = function(counts, summary_path, tissue_categories,
                  pixels_per_micron=getOption('phenoptr.pixels.per.micron')) {

  stopifnot('Slide ID' %in% names(counts),
            'Tissue Category' %in% names(counts))

  # Read the summary data, extract the columns we need, recode the
  # `Tissue Category` total name to match what `count_phenotypes` gives us,
  # and aggregate by `Slide ID` and `Tissue Category`
  summary_data = purrr::map_dfr(summary_path, phenoptr::read_cell_seg_data,
                                              pixels_per_micron=pixels_per_micron) %>%
    dplyr::select(`Slide ID`, `Tissue Category`, Phenotype,
                  `Tissue Area`=`Tissue Category Area (sq microns)`) %>%
    dplyr::filter(Phenotype=='All',
                  `Tissue Category` %in% tissue_categories) %>%
    dplyr::group_by(`Slide ID`, `Tissue Category`) %>%
    dplyr::summarize(`Tissue Area` = sum(`Tissue Area`)/1e6) %>%
    add_tissue_category_totals(tissue_categories) %>%
    dplyr::mutate(`Tissue Category` =
                    dplyr::recode(`Tissue Category`, 'All'='Total'))

  # Check that the files match
  missing_ids = setdiff(unique(counts$`Slide ID`), unique(summary_data$`Slide ID`))
  if (length(missing_ids) > 1)
    stop(length(missing_ids), ' Slide IDs are missing from summary file.')

  # Join with the counts table and divide counts by area to get density
  densities = dplyr::left_join(counts, summary_data) %>%
    dplyr::select(`Slide ID`, `Tissue Category`, `Tissue Area`, dplyr::everything()) %>%
    dplyr::mutate_at(-(1:3), function(x) x/.$`Tissue Area`) %>%
    dplyr::rename(`Tissue Area (mm2)`=`Tissue Area`)

  densities
}


