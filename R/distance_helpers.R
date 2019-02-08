# Helpers for spatial statistics

# Suppress CMD CHECK notes for things that look like global vars
if (getRversion() >= "2.15.1")
  utils::globalVariables(c('Cell X Position', 'Cell Y Position', 'From', 'To'))

#' Summarize nearest neighbor distances
#'
#' Computes summary nearest neighbor statistics (mean, median, etc)
#' for each Slide ID in `csd` and each pair of phenotypes in `phenotypes`.
#' @param csd Cell seg data with `Cell X Position`,
#'        `Cell Y Position`, field name and `Phenotype` columns.
#' @param phenotypes Optional list of phenotypes to include. If omitted,
#' will use `unique_phenotypes(csd)`.
#' @return A data frame with summary statistics for each phenotype pair
#' in each Slide ID.
#' @export
#' @importFrom magrittr %>%
nearest_neighbor_summary = function(csd, phenotypes=NULL) {
  # Make sure phenotypes is a named vector; we will use the names later.
  if (is.null(phenotypes))
    phenotypes = phenoptr::unique_phenotypes(csd)
  if (!rlang::is_named(phenotypes))
    phenotypes = rlang::set_names(phenotypes)

  field_col = rlang::sym(field_column(csd))

  # Compute nearest neighbor distances for all cells, one field at a time.
  distances <- csd %>%
    dplyr::select(`Cell X Position`, `Cell Y Position`, `Slide ID`,
                  !!field_col, dplyr::starts_with('Phenotype')) %>%
    dplyr::group_by(`Slide ID`, !!field_col) %>%
    tidyr::nest() %>%
    dplyr::mutate(distance=purrr::map(data,
                           phenoptr::find_nearest_distance, phenotypes)) %>%
    tidyr::unnest()

  # All pairs of phenotypes. Order matters so this will include both
  # (a, b) and (b, a).
  pheno_pairs = purrr::cross2(names(phenotypes), names(phenotypes))

  # Helper functions for computing a bunch of summary stats on distances
  # Compute summary stats for a single dataset and all pheno_pairs
  summarize_all_pairs = function(d) {
    purrr::map_dfr(pheno_pairs, ~summarize_pair(d, .x[[1]], .x[[2]]))
  }

  # Compute summary stats for a single dataset and a single (from, to) pair
  summarize_pair = function(d, from, to) {
    dist_to_col = paste0('Distance to ', to)

    # Get the correct rows and column and summarize
    d %>% dplyr::filter(phenoptr::select_rows(., phenotypes[[from]])) %>%
      dplyr::pull(dist_to_col) %>% # Single column of interest
      dist_summary %>% # Compute
      dplyr::mutate(From=from, To=to) %>%
      dplyr::select(From, To, dplyr::everything()) # Re-order columns
  }

  # Create a summary data frame from a single vector
  dist_summary = function(v) {
    tibble::tibble(Min=min(v, na.rm=TRUE),
                   Mean=mean(v, na.rm=TRUE),
                   Median=stats::median(v, na.rm=TRUE),
                   Max=max(v, na.rm=TRUE),
                   SD=stats::sd(v, na.rm=TRUE))
  }

  # Now we can do the work, computing summary stats for all pairs,
  # grouping by slides
  distances %>% tidyr::nest(-`Slide ID`) %>%
    dplyr::mutate(stats=purrr::map(data, summarize_all_pairs)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()
}
