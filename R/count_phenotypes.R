# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "Slide ID",
  "Tissue Category",
  "."
))

#' Count phenotypes per slide and tissue category
#'
#' Count selected phenotypes, aggregating by Slide ID and Tissue Category.
#' If multiple tissue categories are selected, include a Total row for
#' each Slide ID.
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param phenotypes A named list of phenotype selectors
#'   (see [parse_phenotypes]). Results will be
#'   reported in the same order as phenotypes are listed here. To include a
#'   row total, `phenotypes` should contain a "Total" item.
#' @param tissue_categories A character vector of tissue category names
#'   of interest.
#' @return A data frame with columns for Slide ID, Tissue Category, cell
#'   counts for each requested phenotype, and total cells.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
count_phenotypes = function(csd, phenotypes, tissue_categories) {
  # Remove rows with no phenotype information so they don't get counted as
  # negative phenotypes. (We don't know what they are, don't assume.)
  # Any phenotype column works for this.
  pheno_col = purrr::detect(names(csd), ~startsWith(.x, 'Phenotype'))
  csd_good = csd[csd[[pheno_col]]!='',] %>%
    dplyr::filter(`Tissue Category` %in% tissue_categories)

  # Make a data frame with a boolean column for each phenotype
  selections = purrr::map_dfc(phenotypes,
                   function(pheno) phenoptr::select_rows(csd_good, pheno))

  selections = csd_good %>%
    dplyr::select(`Slide ID`, `Tissue Category`) %>%
    bind_cols(selections)

  # Make the `fill` argument to `tidyr::complete`; we want 0 for missing values.
  pheno_names = names(phenotypes)
  fill=rep(0, length(pheno_names)) %>% as.list %>% rlang::set_names(pheno_names)

  # Count the positive selections and fill in
  # missing Slide ID / Tissue Category pairs
  selections = selections %>% dplyr::group_by(`Slide ID`, `Tissue Category`) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::ungroup() %>%
    tidyr::complete(`Slide ID`, `Tissue Category`, fill=fill)

  # Compute totals per tissue category if there are multiple categories
  add_tissue_category_totals(selections, tissue_categories)
}
