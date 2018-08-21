# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "Slide ID",
  "Tissue Category",
  "."
))

#' Count phenotypes per slide and tissue category
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param phenotypes A named list of phenotype selectors. Results will be
#'   reported in the same order as phenotypes are listed here.
#' @param tissue_categories A character vector of tissue category names
#'   of interest.
#' @return A data frame with columns for Slide ID, Tissue Category, cell
#'   counts for each requested phenotype, and total cells
#' @importFrom magrittr %>%
#' @export
count_phenotypes = function(csd, phenotypes, tissue_categories) {
  # Remove rows with no phenotype information so they don't show up as
  # negative. Any phenotype column works for this.
  pheno_col = purrr::detect(names(csd), ~startsWith(.x, 'Phenotype'))
  selections = csd[csd[[pheno_col]]!='',]

  # Make a data frame with a boolean column for each phenotype
  selections = selections %>%
    dplyr::select(`Slide ID`, `Tissue Category`, dplyr::starts_with('Phenotype')) %>%
    dplyr::filter(`Tissue Category` %in% tissue_categories) %>%
    dplyr::bind_cols(purrr::map_dfc(phenotypes,
                             function(pheno) phenoptr::select_rows(., pheno))) %>%
    dplyr::select(-dplyr::starts_with('Phenotype'))

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
