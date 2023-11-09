# Helpers for spatial statistics

# Suppress CMD CHECK notes for things that look like global vars
if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c('Cell X Position', 'Cell Y Position', 'From', 'To',
      'category', 'from', 'to', 'radius', 'from_count', 'within_mean',
      'to_count', 'from_with', 'data', 'distance', 'stats',
      'Radius', 'count_summary'))

#' Summarize nearest neighbor distances
#'
#' Computes summary nearest neighbor statistics (mean, median, etc)
#' for each `.by` in `csd` and each pair of phenotypes in `phenotypes`.
#' Statistics are computed separately for each category in `categories`
#' and, if multiple categories are provided, once for all included cells.

#' Note: If `categories` is `NA` or includes `NA`, the "All" tissue category in
#' the results will include all cells in `csd`. If `categories` does not include
#' `NA`, the "All" tissue category will include only cells in the provided
#' categories.
#'
#' @param csd Cell seg data with `Cell X Position`,
#'        `Cell Y Position`, field name and `Phenotype` columns.
#' @param phenotypes Optional list of phenotypes to include. If omitted,
#' will use `unique_phenotypes(csd)`.
#' @param categories Optional list of tissue categories to compute within. If
#' omitted or `NA`, all cells will be included.
#' @param details_path If present, path to save a tab-separated table
#' for each tissue category containing
#' nearest-neighbor data for each cell in the tissue category.
#' @param .by Column to aggregate by
#' @param extra_cols The names of extra columns to include in the detailed
#' results.
#' @return A data frame with summary statistics for each phenotype pair
#' in each Slide ID for each tissue category.
#' @export
#' @importFrom magrittr %>%
nearest_neighbor_summary = function(csd, phenotypes=NULL,
                                    categories=NA, details_path=NULL,
                                    .by='Slide ID', extra_cols=NULL) {
  # Prep parameters
  phenotypes = phenoptr::validate_phenotypes(phenotypes, csd)
  extra_cols = c(unlist(extra_cols),
                 phenoptr::phenotype_columns(phenotypes)) %>%
    unique() %>%
    sort()

  field_col = rlang::sym(phenoptr::field_column(csd))
  .by = rlang::sym(.by)

  # If no categories, compute once for all cells
  if (all(is.na(categories))) {
    result = nearest_neighbor_summary_single_impl(csd, phenotypes,
                                           categories, details_path,
                                           .by, field_col, extra_cols)
    return(result)
  }

  # Compute once for each category and once for all supplied categories
  result = list()
  for (category in categories) {
    # Assume that details_path ends with .txt and make a path per category
    category_path = stringr::str_replace(details_path,
                      '\\.txt$', paste0('_', category, '.txt'))
    category_result = nearest_neighbor_summary_single_impl(csd, phenotypes,
                        category, category_path,
                        .by, field_col, extra_cols) %>%
      dplyr::mutate(`Tissue Category`=category)
    result = c(result, list(category_result))
  }

  if (length(categories) > 1 && !any(is.na(categories))) {
    category_path = stringr::str_replace(details_path,
                                         '\\.txt$', '_All.txt')
    category_result = nearest_neighbor_summary_single_impl(csd, phenotypes,
                        categories, category_path,
                        .by, field_col, extra_cols) %>%
      dplyr::mutate(`Tissue Category`='All')
    result = c(result, list(category_result))
  }

  # Row-bind and re-order columns
  result = result %>%
    dplyr::bind_rows() %>%
    dplyr::select(!!.by, `Tissue Category`, dplyr::everything())

  # Re-order rows
  result =
    order_by_slide_phenotype_category(result, .by, categories, phenotypes)

  result
}

# Internal implementation of nearest_neighbor_summary for a single
# category (or categories), from cleaned parameters.
nearest_neighbor_summary_single_impl = function(csd, phenotypes,
                                         categories, details_path,
                                         .by, field_col, extra_cols) {

  # Filter by category if provided
  if (!any(is.na(categories)))
      csd = csd %>%
        dplyr::filter(`Tissue Category` %in% categories)

  # Prep data - nest csd by field and .by
  if (.by == field_col)
    csd_nested = csd %>% dplyr::group_by(!!.by) %>% tidyr::nest()
  else
    csd_nested = csd %>% dplyr::group_by(!!.by, !!field_col) %>% tidyr::nest()

  # Compute nearest neighbor distances for all cells in categories,
  # one field at a time.
  distances = csd_nested %>%
    dplyr::mutate(distance=purrr::map(data,
                    phenoptr::find_nearest_distance, phenotypes)) %>%
    tidyr::unnest(cols=c(data, distance)) %>%
    dplyr::ungroup()

  # Optionally save details with a subset of columns
  if (!is.null(details_path) && length(details_path) > 0 && nrow(distances) > 0) {
    distances_subset = distances %>%
      dplyr::select(!!.by, `Cell ID`, `Cell X Position`, `Cell Y Position`,
                    !!field_col,
                    dplyr::contains('Tissue Category'),
                    tidyselect::any_of(extra_cols),
                    dplyr::starts_with('Phenotype'),
                    # This preserves order of Distance and Cell ID columns
                    dplyr::matches('Distance to |Cell ID '),
                    # Include any TMA columns
                    dplyr::starts_with('TMA '))
    readr::write_tsv(distances_subset, details_path, na='#N/A')
  }

  # All pairs of phenotypes. Order matters so this will include both
  # (a, b) and (b, a).
  pheno_pairs <- tidyr::expand_grid(A = names(phenotypes), B = names(phenotypes)) %>%
    split(seq_len(nrow(.))) %>% 
    map(~ .x %>% as.list())

  # Helper functions for computing a bunch of summary stats on distances
  # Compute summary stats for a single dataset and all pheno_pairs
  summarize_all_pairs = function(d) {
    purrr::map_dfr(pheno_pairs, ~summarize_pair(d, .x[[1]], .x[[2]]))
  }

  # Compute summary stats for a single dataset and a single (from, to) pair
  summarize_pair = function(d, from, to) {
    dist_to_col = paste0('Distance to ', to)

    # Get the correct rows and column and summarize
    d %>%
      dplyr::filter(phenoptr::select_rows(., phenotypes[[from]])) %>%
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
  distances %>%
    tidyr::nest(data = c(-(!!.by))) %>%
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
#' the count of cells of that type within that distance.
#'
#' Note: If `categories` is `NA` or includes `NA`, the "All" tissue category in
#' the results will include all cells in `csd`. If `categories` does not include
#' `NA`, the "All" tissue category will include only cells in the provided
#' categories.
#'
#' @param csd Cell seg data with `Cell X Position`,
#'        `Cell Y Position`, field name and `Phenotype` columns.
#' @param radii Vector of radii to search within.
#' @param phenotypes Optional list of phenotypes to include. If omitted,
#' will use `unique_phenotypes(csd)`. Counts are computed for all
#' pairs of phenotypes.
#' @param categories Optional list of tissue categories to compute within. If
#' omitted or `NA`, all cells will be included.
#' @param details_path If present, path to save tab-separated tables with
#' count within data for each cell.
#' @param .by Column to aggregate by
#' @param extra_cols The names of extra columns to include in the detailed
#' results.
#' @return A data frame with summary statistics for each phenotype pair
#' in each Slide ID.
#' @export
#' @importFrom magrittr %>%
count_within_summary = function(csd, radii, phenotypes=NULL, categories=NA,
                                details_path=NULL, .by='Slide ID',
                                extra_cols=NULL) {
  phenotypes = phenoptr::validate_phenotypes(phenotypes, csd)
  extra_cols = c(unlist(extra_cols),
                   phenoptr::phenotype_columns(phenotypes)) %>%
    unique() %>%
    sort()

  # The column name that defines fields
  field_col = rlang::sym(phenoptr::field_column(csd))
  .by = rlang::sym(.by)

  # If no categories, compute once for all cells
  if (all(is.na(categories))) {
    result = count_within_summary_impl(csd, phenotypes, radii,
                                       categories, details_path,
                                       .by, field_col, extra_cols)
    return(result)
  }

  # Compute once for each category and once for all supplied categories
  result = list()
  for (category in categories) {
    # Assume that details_path ends with .txt and make a path per category
    category_path = stringr::str_replace(details_path,
                                         '\\.txt$', paste0('_', category, '.txt'))
    category_result = count_within_summary_impl(csd, phenotypes, radii,
                                                category, category_path,
                                                .by, field_col, extra_cols)
    result = c(result, list(category_result))
  }

  if (length(categories) > 1 && !any(is.na(categories))) {
    category_path = stringr::str_replace(details_path,
                                         '\\.txt$', '_All.txt')
    category_result = count_within_summary_impl(csd, phenotypes, radii,
                                                categories, category_path,
                                                .by, field_col, extra_cols)
    result = c(result, list(category_result))
  }

  # Row-bind, clean up names and re-order
  result = result %>%
    dplyr::bind_rows() %>%
    # Uppercase 'All'
    dplyr::mutate(category=ifelse(category=='all', 'All', category)) %>%
    # Make pretty names for the Excel export
    dplyr::rename(`Tissue Category` = category,
                  From=from, To=to, Radius=radius,
                  `From count` = from_count,
                  `To count` = to_count,
                  `From with` = from_with,
                  `Within mean` = within_mean) %>%
    # Re-order columns
    dplyr::select(!!.by, `Tissue Category`, dplyr::everything()) %>%
    # Re-order rows
    order_by_slide_phenotype_category(.by, categories, phenotypes, Radius)

  result
}

# Internal implementation of count_within_summary for a single category
# (or categories) from cleaned parameters
count_within_summary_impl = function(csd, phenotypes, radii,
                                     categories, category_path,
                                     .by, field_col, extra_cols) {
  # Filter by categories if provided
  if (!any(is.na(categories)))
    csd = csd %>%
      dplyr::filter(`Tissue Category` %in% categories)

  # All pairs of phenotypes as a list of vectors.
  # Order matters so this will include both (a, b) and (b, a).
  pheno_pairs <- expand_grid(A = names(phenotypes), B = names(phenotypes)) %>%
    split(seq_len(nrow(.))) %>% 
    map(~ unlist(.x))


  # Prep data - nest csd by field and .by
  if (.by == field_col)
    csd_nested = csd %>% dplyr::group_by(!!.by) %>% tidyr::nest()
  else
    csd_nested = csd %>% dplyr::group_by(!!.by, !!field_col) %>% tidyr::nest()

  # Compute counts for all cells in categories, one field at a time.
  counts = csd_nested %>%
    dplyr::mutate(counts=purrr::map(data,
                    phenoptr::count_within_detail, phenotypes, radii)) %>%
    tidyr::unnest(cols=c(data, counts)) %>%
    dplyr::ungroup()

  # Optionally save details with a subset of columns
  if (!is.null(category_path) && length(category_path) > 0 && nrow(counts) > 0) {
    counts_subset = counts %>%
      dplyr::select(!!.by, `Cell ID`, `Cell X Position`, `Cell Y Position`,
                    !!field_col,
                    dplyr::contains('Tissue Category'),
                    tidyselect::any_of(extra_cols),
                    dplyr::starts_with('Phenotype'),
                    dplyr::contains('within'),
                    dplyr::starts_with('TMA '))
    readr::write_tsv(counts_subset, category_path, na='#N/A')
  }

  # Summarize counts for one `.by` grouping and all pairs
  # summarize_combo does the work
  # This function breaks up the nested map in the calculation of counts_summary
  # summarize_combo is going to filter by category, we have to give it something
  summarize_category = ifelse(length(categories)>1, NA, categories)
  summarize_group = function(d) {
    purrr::map_dfr(pheno_pairs, function(pheno_pair) {
      phenoptr:::summarize_combo(d, summarize_category, phenotypes,
                                 pheno_pair[[1]], pheno_pair[[2]], radii)
    })
  }

  # Now we can compute summary stats for all combos and radii for each .by
  counts_summary = counts %>%
    dplyr::group_by(!!.by) %>%
    tidyr::nest() %>%
    # Summarize each group
    dplyr::mutate(count_summary=purrr::map(data, summarize_group)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(count_summary)

  counts_summary
}
