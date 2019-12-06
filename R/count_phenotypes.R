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
#'   (see [phenoptr::parse_phenotypes]). Results will be
#'   reported in the same order as phenotypes are listed here. To include a
#'   row total, `phenotypes` should contain a "Total" item.
#' @param tissue_categories A character vector of tissue category names
#'   of interest.
#' @param .by Column to aggregate by
#' @return A data frame with columns for Slide ID, Tissue Category, cell
#'   counts for each requested phenotype, and total cells.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
count_phenotypes = function(csd, phenotypes, tissue_categories,
                            .by='Slide ID') {
  if (!.by %in% names(csd))
    stop('No ', .by, ' column in cell seg data.')
  .by = rlang::sym(.by)

  # Remove rows with no phenotype information so they don't get counted as
  # negative phenotypes. (We don't know what they are, don't assume.)
  # Any phenotype column works for this.
  pheno_col = purrr::detect(names(csd), ~startsWith(.x, 'Phenotype'))
  csd_good = csd[csd[[pheno_col]]!='', ] %>%
    dplyr::filter(`Tissue Category` %in% tissue_categories)

  # Make a data frame with a boolean column for each phenotype
  selections = purrr::map_dfc(phenotypes,
                   function(pheno) phenoptr::select_rows(csd_good, pheno))

  selections = csd_good %>%
    dplyr::select(!!.by, `Tissue Category`) %>%
    dplyr::bind_cols(selections)

  # Make the `fill` argument to `tidyr::complete`; we want 0 for missing values.
  pheno_names = names(phenotypes)
  fill=rep(0, length(pheno_names)) %>% as.list %>% rlang::set_names(pheno_names)

  # Count the positive selections and fill in
  # missing .by / Tissue Category pairs
  selections = selections %>%
    dplyr::group_by(!!.by, `Tissue Category`) %>%
    dplyr::summarize_all(sum, na.rm=TRUE) %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!.by, `Tissue Category`, fill=fill)

  # Compute totals per tissue category if there are multiple categories
  add_tissue_category_totals(selections, tissue_categories, .by=.by)
}

#' Convert a count table to fractional percents
#'
#' Converts a table of counts, such as the output of [count_phenotypes],
#' to a table of fractional percents. Percents are computed row-wise, e.g. per
#' tissue category.
#' @param counts A table containing count data. All numeric columns are
#'   assumed to contain count data. The table must contain a
#'   column whose name contains "Total" or "All".
#' @return A table containing percent values as decimal fractions.
#' @export
#' @family aggregation functions
#' @importFrom magrittr %>%
counts_to_percents = function(counts) {
  # Figure out the column to total on
  if ('Total Cells' %in% names(counts)) {
    column = 'Total Cells'
  } else {
    candidates = stringr::str_detect(names(counts), 'Total|All')
    if (sum(candidates)==1) {
      column = names(counts)[candidates]
    } else
      stop('Count table must contain a "Total" or "All" column.')
  }

  # Compute percents using NA for missing values
  totals = counts[[column]]
  counts %>% dplyr::mutate_if(is.numeric, ~ifelse(totals==0, NA, .x/totals))
}
