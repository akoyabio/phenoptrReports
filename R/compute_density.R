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
#' @param summary_path Path to a cell seg data summary table containing
#'   sample names and tissue categories matching `counts`.
#' @param pixels_per_micron Conversion factor to microns.
#' @return A data table with counts converted to density in
#'   \ifelse{html}{\out{mm<sup>2</sup>}{\eqn{cells / mm^2}}.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_density = function(counts, summary_path,
                  pixels_per_micron=getOption('phenoptr.pixels.per.micron')) {

  stopifnot('Slide ID' %in% names(counts),
            'Tissue Category' %in% names(counts))

  # Read the summary data, extract the columns we need, recode the
  # `Tissue Category` total name to match what `count_phenotypes` gives us,
  # and aggregate by `Slide ID` and `Tissue Category`
  summary_data = phenoptr::read_cell_seg_data(summary_path,
                                              pixels_per_micron=pixels_per_micron) %>%
    dplyr::select(`Slide ID`, `Tissue Category`, Phenotype,
                  `Tissue Area`=`Tissue Category Area (sq microns)`) %>%
    dplyr::filter(Phenotype=='All') %>%
    dplyr::mutate(`Tissue Category` =
                    dplyr::recode(`Tissue Category`, 'All'='Total')) %>%
    dplyr::group_by(`Slide ID`, `Tissue Category`) %>%
    dplyr::summarize(`Tissue Area` = sum(`Tissue Area`)/1e6)

  # Join with the counts table and divide counts by area to get density
  densities = dplyr::left_join(counts, summary_data) %>%
    dplyr::select(`Slide ID`, `Tissue Category`, `Tissue Area`, dplyr::everything()) %>%
    dplyr::mutate_at(-(1:3), function(x) x/.$`Tissue Area`) %>%
    dplyr::rename(`Tissue Area (mm2)`=`Tissue Area`)
}


