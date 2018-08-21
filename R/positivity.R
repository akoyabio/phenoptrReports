# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  '1+', '2+', '3+',
  'Count of 0+', 'Count of 1+', 'Count of 2+', 'Count of 3+',
  'Total', 'score'
))

#' Compute positivity of multiple phenotypes
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param positivity_pairs A named list of pairs (lists) of phenotype selectors
#'   and positivity expressions.
#' @return A data frame with columns for count,
#'   positive count,
#'   and percent for each element of `positivity_pairs`.
#' @family aggregation functions
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
#' @family aggregation functions
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

#' Compute H-score for a single marker aggregated by Slide ID
#'
#' Thresholds may come from a score data file or be directly provided.
#'
#' @param csd Cell seg data to use.
#' @param measure The cell seg data column to measure (as a character string)
#' @param tissue_categories A character vector of tissue category names
#'   of interest.
#' @param score_path Optional path to to a score_data file (may be merged or not).
#' @param thresholds Optional three element vector with the threshold values.
#' @return A data frame with one row per Slide ID, showing cell counts and
#'   percents in each bin and the H score
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_h_score = function(csd, measure, tissue_categories,
                           score_path=NULL, thresholds=NULL) {
  if (!measure %in% names(csd))
    stop("Column not found: ", measure)

  if (is.null(score_path) == is.null(thresholds))
    stop("You must provide one of 'score_path' or 'thresholds', and not both.")

  measure = rlang::sym(measure)

  # We only need three columns
  d = csd %>% dplyr::select(`Slide ID`, `Tissue Category`, !!measure) %>%
    dplyr::filter(`Tissue Category` %in% tissue_categories)

  # Read the score data to get the thresholds if they were not directly provided
  if (!is.null(score_path)) {
    score_data = readr::read_tsv(score_path, n_max=1)
    score_names = c('Threshold 0/1+', 'Threshold 1+/2+', 'Threshold 2+/3+')
    if (!all(score_names %in% names(score_data)))
      stop('"', score_path, '" does not seem to be an H-score data file.')

    thresholds = score_data[,score_names] %>% purrr::flatten_dbl()
  }

  d = d %>% dplyr::mutate(score = dplyr::case_when(
    !!measure < thresholds[1] ~ 0,
    !!measure < thresholds[2] ~ 1,
    !!measure < thresholds[3] ~ 2,
    TRUE ~ 3)) %>%
    dplyr::group_by(`Slide ID`, `Tissue Category`) %>%
    dplyr::summarize(
      `Count of 0+` = sum(score==0),
      `Count of 1+` = sum(score==1),
      `Count of 2+` = sum(score==2),
      `Count of 3+` = sum(score==3),
      Total = dplyr::n()
    ) %>% dplyr::ungroup()

  d = add_tissue_category_totals(d, tissue_categories) %>%
    dplyr::mutate(
      `0+` = round(`Count of 0+`/Total, 2),
      `1+` = round(`Count of 1+`/Total, 2),
      `2+` = round(`Count of 2+`/Total, 2),
      `3+` = round(`Count of 3+`/Total, 2),
      `H-Score` = as.integer(100 * (`1+` + 2*`2+` + 3*`3+`))
    )
  d
}
