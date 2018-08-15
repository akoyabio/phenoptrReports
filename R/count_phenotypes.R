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
  selections = csd %>%
    dplyr::select(`Slide ID`, `Tissue Category`, dplyr::starts_with('Phenotype')) %>%
    dplyr::filter(`Tissue Category` %in% tissue_categories) %>%
    dplyr::bind_cols(purrr::map_dfc(phenotypes,
                             function(pheno) phenoptr::select_rows(., pheno))) %>%
    dplyr::select(-dplyr::starts_with('Phenotype '))

  pheno_names = names(phenotypes)

  fill=rep(0, length(pheno_names)) %>% as.list %>% rlang::set_names(pheno_names)

  selections = selections %>% dplyr::group_by(`Slide ID`, `Tissue Category`) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::ungroup() %>%
    tidyr::complete(`Slide ID`, `Tissue Category`, fill=fill)

  # Compute totals per phenotype if there are multiple tissue categories
  if (length(tissue_categories) > 1) {
    totals = selections %>% dplyr::group_by(`Slide ID`) %>%
      dplyr::summarize_at(dplyr::vars(-(1:2)), sum)
    totals$`Tissue Category` = 'Total'
    result = dplyr::bind_rows(selections, totals)
  } else result = selections

  # Lookup table for ordering tissue categories
  tissue_order = 1:(length(tissue_categories)+1) %>%
    rlang::set_names(c(tissue_categories, 'Total'))
   result %>%
    dplyr::arrange(`Slide ID`, tissue_order[`Tissue Category`])
}
