# Helpers for spatial statistics

# Suppress CMD CHECK notes for things that look like global vars
if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c('Cell X Position', 'Cell Y Position', 'From', 'To',
      'category', 'from', 'to', 'radius', 'from_count', 'within_mean',
      'to_count', 'from_with'))

#' Summarize nearest neighbor distances
#'
#' Computes summary nearest neighbor statistics (mean, median, etc)
#' for each Slide ID in `csd` and each pair of phenotypes in `phenotypes`.
#' @param csd Cell seg data with `Cell X Position`,
#'        `Cell Y Position`, field name and `Phenotype` columns.
#' @param phenotypes Optional list of phenotypes to include. If omitted,
#' will use `unique_phenotypes(csd)`.
#' @param details_path If present, path to save a table with
#' nearest-neighbor data for each cell.
#' @return A data frame with summary statistics for each phenotype pair
#' in each Slide ID.
#' @export
#' @importFrom magrittr %>%
nearest_neighbor_summary = function(csd, phenotypes=NULL, details_path=NULL) {
  phenotypes = phenoptr::validate_phenotypes(phenotypes, csd)

  field_col = rlang::sym(phenoptr::field_column(csd))

  # Compute nearest neighbor distances for all cells, one field at a time.
  distances <- csd %>%
    dplyr::select(`Slide ID`, `Cell ID`, `Cell X Position`, `Cell Y Position`,
                  !!field_col, dplyr::starts_with('Phenotype')) %>%
    dplyr::group_by(`Slide ID`, !!field_col) %>%
    tidyr::nest() %>%
    dplyr::mutate(distance=purrr::map(data,
                           phenoptr::find_nearest_distance, phenotypes)) %>%
    tidyr::unnest()

  # Optionally save details
  if (!is.null(details_path))
    readr::write_csv(distances, details_path, na='#N/A')

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

#' Summarize "count within" distances
#'
#' Computes summary "count within" statistics
#' for each Slide ID in `csd` and each pair of phenotypes in `phenotypes`.
#' See [phenoptr::count_within()] for details of the counts and the
#' summary calculation.
#' @param csd Cell seg data with `Cell X Position`,
#'        `Cell Y Position`, field name and `Phenotype` columns.
#' @param radii Vector of radii to search within.
#' @param phenotypes Optional list of phenotypes to include. If omitted,
#' will use `unique_phenotypes(csd)`. Counts are computed for all
#' pairs of phenotypes.
#' @param categories Optional list of tissue categories to compute within.
#' @return A data frame with summary statistics for each phenotype pair
#' in each Slide ID.
#' @export
#' @importFrom magrittr %>%
count_within_summary = function(csd, radii, phenotypes=NULL, categories=NULL) {
  phenotypes = phenoptr::validate_phenotypes(phenotypes, csd)

  # The column name that defines fields
  field_col = rlang::sym(phenoptr::field_column(csd))

  # All pairs of phenotypes as a list of vectors.
  # Order matters so this will include both (a, b) and (b, a).
  pheno_pairs = purrr::cross2(names(phenotypes), names(phenotypes)) %>%
    purrr::map(unlist)

  # Compute count_within for each field.
  distances <- csd %>%
    # Select columns of interest and nest per field
    dplyr::select(`Cell X Position`, `Cell Y Position`,
                  `Slide ID`, `Tissue Category`,
                  !!field_col, dplyr::starts_with('Phenotype')) %>%
    dplyr::group_by(`Slide ID`, !!field_col) %>%
    tidyr::nest() %>%
    # The actual calculation.
    # count_within_many handles multiple pairs, radii and tissue categories.
    dplyr::mutate(within=purrr::map(data, phenoptr::count_within_many,
                pheno_pairs, radii, categories, phenotypes, verbose=FALSE)) %>%
    # Unnest and cleanup
    dplyr::select(-data) %>%
    tidyr::unnest() %>%
    dplyr::select(-source) # Chaff from count_within_many

  # Aggregate per slide. See ?phenoptr::count_within for explanation
  distances %>% dplyr::group_by(`Slide ID`, category, from, to, radius) %>%
    dplyr::summarize(within=sum(from_count*within_mean),
              from_count=sum(from_count),
              to_count=sum(to_count),
              from_with=sum(from_with),
              within_mean=within/from_count) %>%
    dplyr::select(-within) %>%
    dplyr::ungroup() %>%
    # Make pretty names for the Excel export
    dplyr::rename(`Tissue Category` = category,
                  From=from, To=to, Radius=radius,
                  `From count` = from_count,
                  `To count` = to_count,
                  `From with` = from_with,
                  `Within mean` = within_mean)

}
