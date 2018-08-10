#' Compute positivity of multiple phenotypes
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param positivity_pairs A named list of pairs (lists) of phenotype selectors
#'   and positivity expressions.
#' @return A data frame with columns for count,
#'   positive count,
#'   and percent for each element of `positivity_pairs`.
#' @importFrom rlang !! :=
#' @export
compute_positivity_many = function(csd, positivity_pairs) {
  purrr::map(names(positivity_pairs), ~{
    name = .x
    phenotype = positivity_pairs[[name]][[1]]
    positivity = positivity_pairs[[name]][[2]]
    d = compute_positivity(csd, phenotype, positivity)
    count_name = paste(paste(phenotype, collapse='/'), 'Count')
    pos_name = paste(name, 'Count')
    pct_name = paste(name, 'Pct')
    d %>% dplyr::rename(!!count_name := count, !!pos_name := positive,
                        !!pct_name := fraction)
  }) %>% dplyr::bind_cols()
}

#' Compute positivity of a single phenotype
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param phenotype A phenotype selector. This will be passed to
#'   \code{\link[phenoptr]{select_rows}}.
#' @param positivity A one-sided formula giving the positivity expression
#'   to evaluate.
#' @return A data frame with columns for count (total number of cells of
#'   the given phenotype),
#'   positive (count of positive cells)
#'   and fraction (fraction of positive cells).
#' @export
compute_positivity = function(csd, phenotype, positivity) {
  # Parameter checking
  stopifnot(is.data.frame(csd))

  # Select just the cells of interest
  d = csd[phenoptr::select_rows(csd, phenotype),]
  count = nrow(d) # Count of all positive

  d = d[phenoptr::select_rows(d, positivity),]
  positive = nrow(d)
  tibble::data_frame(count=count, positive=positive, fraction=positive/count)
}
