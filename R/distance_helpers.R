# Helpers for spatial statistics

# Suppress CMD CHECK notes for things that look like global vars
if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c('Cell X Position', 'Cell Y Position', 'From', 'To',
      'category', 'from', 'to', 'radius', 'from_count', 'within_mean',
      'to_count', 'from_with', 'data', 'distance', 'stats'))

#' Summarize nearest neighbor distances
#'
#' Computes summary nearest neighbor statistics (mean, median, etc)
#' for each `.by` in `csd` and each pair of phenotypes in `phenotypes`.
#' @param csd Cell seg data with `Cell X Position`,
#'        `Cell Y Position`, field name and `Phenotype` columns.
#' @param phenotypes Optional list of phenotypes to include. If omitted,
#' will use `unique_phenotypes(csd)`.
#' @param details_path If present, path to save a tab-separated table with
#' nearest-neighbor data for each cell.
#' @param .by Column to aggregate by
#' @return A data frame with summary statistics for each phenotype pair
#' in each Slide ID.
#' @export
#' @importFrom magrittr %>%
nearest_neighbor_summary = function(csd, phenotypes=NULL, details_path=NULL,
                                    .by='Slide ID') {
  phenotypes = phenoptr::validate_phenotypes(phenotypes, csd)

  field_col = rlang::sym(phenoptr::field_column(csd))
  .by = rlang::sym(.by)

  # Compute nearest neighbor distances for all cells, one field at a time.
  distances <- csd %>%
    dplyr::group_by(!!.by, !!field_col) %>%
    tidyr::nest() %>%
    dplyr::mutate(distance=purrr::map(data,
                           phenoptr::find_nearest_distance, phenotypes)) %>%
    tidyr::unnest(cols=c(data, distance))

  # Optionally save details with a subset of columns
  if (!is.null(details_path)) {
    distances_subset = distances %>%
      dplyr::select(!!.by, `Cell ID`, `Cell X Position`, `Cell Y Position`,
                    !!field_col,
                    dplyr::contains('Tissue Category'),
                    dplyr::starts_with('Phenotype'),
                    # This preserves order of Distance and Cell ID columns
                    dplyr::matches('Distance to |Cell ID '))
    readr::write_tsv(distances_subset, details_path, na='#N/A')
  }

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

  # Create a summary data frame from a single vector.
  # If the vector is empty (because there are no examples of a phenotype),
  # min and max are noisy and return Inf and -Inf; mean returns NaN.
  # Check for this and quietly return all NA instead.
  dist_summary = function(v) {
    v = v[!is.na(v)]
    if (length(v) == 0) {
      tibble::tibble(Min=NA,
                     Mean=NA,
                     Median=NA,
                     Max=NA,
                     SD=NA)
    } else {
      tibble::tibble(Min=min(v, na.rm=TRUE),
                     Mean=mean(v, na.rm=TRUE),
                     Median=stats::median(v, na.rm=TRUE),
                     Max=max(v, na.rm=TRUE),
                     SD=stats::sd(v, na.rm=TRUE))
    }
  }

  # Now we can do the work, computing summary stats for all pairs,
  # grouping by .by
  distances %>% tidyr::nest(data = c(-(!!.by))) %>%
    dplyr::mutate(stats=purrr::map(data, summarize_all_pairs)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(stats)
}

#' Summarize "count within" distances
#'
#' Computes summary "count within" statistics
#' for each Slide ID in `csd` and each pair of phenotypes in `phenotypes`.
#' See [phenoptr::count_within()] for details of the counts and the
#' summary calculation.
#'
#' If `details_path` is provided, this will save a table with one
#' row per cell and columns for each phenotype and radius giving
#' the count of cells of that type within that distance. The detail table
#' is computed without regard to tissue category so it may not exactly
#' match the summary table. Details are only saved if the `akoyabio/rtree`
#' package is available.
#' @param csd Cell seg data with `Cell X Position`,
#'        `Cell Y Position`, field name and `Phenotype` columns.
#' @param radii Vector of radii to search within.
#' @param phenotypes Optional list of phenotypes to include. If omitted,
#' will use `unique_phenotypes(csd)`. Counts are computed for all
#' pairs of phenotypes.
#' @param categories Optional list of tissue categories to compute within.
#' @param details_path If present, path to save a tab-separated table with
#' nearest-neighbor data for each cell.
#' @param .by Column to aggregate by
#' @return A data frame with summary statistics for each phenotype pair
#' in each Slide ID.
#' @export
#' @importFrom magrittr %>%
count_within_summary = function(csd, radii, phenotypes=NULL, categories=NA,
                                details_path=NULL, .by='Slide ID') {
  phenotypes = phenoptr::validate_phenotypes(phenotypes, csd)

  # The column name that defines fields
  field_col = rlang::sym(phenoptr::field_column(csd))
  .by = rlang::sym(.by)

  # All pairs of phenotypes as a list of vectors.
  # Order matters so this will include both (a, b) and (b, a).
  pheno_pairs = purrr::cross2(names(phenotypes), names(phenotypes)) %>%
    purrr::map(unlist)

  # Nest by field
  nested = csd %>%
    dplyr::group_by(!!.by, !!field_col) %>%
    tidyr::nest()

  # Compute and save the detail table if requested and available
  if (!is.null(details_path)) {
    if (!requireNamespace('rtree', quietly=TRUE))
      warning('count_within details requires the akoyabio/rtree package.')
    else {
      # Just do the calculation again, it is fast enough.
      # This is a bit of a cop-out but surfacing the details from
      # count_within_many is a pain.
      detail = nested %>%
        dplyr::mutate(within=purrr::map(data,
                      phenoptr::count_within_detail, phenotypes, radii)) %>%
        tidyr::unnest(cols=c(data, within)) %>%
        dplyr::select(!!.by, `Cell ID`, `Cell X Position`, `Cell Y Position`,
                      !!field_col,
                      dplyr::contains('Tissue Category'),
                      dplyr::starts_with('Phenotype'),
                      dplyr::contains('within'))
      readr::write_tsv(detail, details_path, na='#N/A')
    }
  }

  # Compute count_within for each field.
  distances <- nested %>%
    # The actual calculation.
    # count_within_many handles multiple pairs, radii and tissue categories.
    dplyr::mutate(within=purrr::map(data, phenoptr::count_within_many,
                pheno_pairs, radii, categories, phenotypes, verbose=FALSE)) %>%
    # Unnest and cleanup
    dplyr::select(-data) %>%
    tidyr::unnest(cols=c(within)) %>%
    dplyr::select(-source) # Chaff from count_within_many

  # Aggregate per .by. See ?phenoptr::count_within for explanation
  distances %>% dplyr::group_by(!!.by, category, from, to, radius) %>%
    dplyr::summarize(within=sum(from_count*within_mean, na.rm=TRUE),
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
