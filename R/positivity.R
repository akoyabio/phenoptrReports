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
#' @param phenotypes A named list of phenotype selectors
#'  (see [phenoptr::parse_phenotypes]).
#' @param positivity_pairs A named list of pairs (lists) of phenotype names
#'   and positivity expressions. The expressions must be one-sided formulas
#'   such as ``~`Membrane PDL1 (Opal 520) Mean`>pdl1_threshold``.
#' @param tissue_categories Optional vector of tissue category names to include.
#' @return A data frame with columns for count,
#'   positive count,
#'   and percent for each element of `positivity_pairs`.
#' @family aggregation functions
#' @importFrom rlang !! :=
#' @export
compute_positivity_many = function(csd, phenotypes, positivity_pairs,
                                   tissue_categories=NULL)
{
  check_phenotypes(purrr::map_chr(positivity_pairs, 1), phenotypes)

  csd = make_nested(csd, tissue_categories)

  # Function to compute all positivities for a single nested data frame
  compute_data_frame = function(d) {
    purrr::map_dfc(names(positivity_pairs), ~{
      # Get the result name, phenotype selector, and positivity expression
      name = .x
      pair = positivity_pairs[[name]]
      phenotype = phenotypes[[pair[[1]]]]
      positivity = pair[[2]]

      # Do the actual calculation
      d = compute_positivity(d, phenotype, positivity)

      # Make nice column names so we can paste them all together
      count_name = paste(paste(phenotype, collapse='/'), 'Count')
      pos_name = paste(name, 'Count')
      pct_name = paste(name, 'Pct')
      d %>% dplyr::rename(!!count_name := count, !!pos_name := positive,
                          !!pct_name := fraction)
    })
  }

  # Actually do the computation for each nested data frame
  csd %>% dplyr::mutate(positivity = purrr::map(data, compute_data_frame)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()
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
  tibble::tibble(count=count, positive=positive, fraction=positive/count)
}

#' Compute H-Score based on parameters in a score data file
#'
#' @param csd Cell seg data to use.
#' @param score_path Path to to a score_data file (may be merged or not).
#' @param tissue_categories optionally specify tissue categories of interest.
#'   If not given, tissue categories are taken from the score file.
#' @param .by Column to aggregate by
#' @return A data frame with one row per Slide ID, showing cell counts and
#'   percents in each bin and the H-Score. See [compute_h_score].
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_h_score_from_score_data = function(csd, score_path,
                                           tissue_categories=NULL,
                                           .by='Slide ID') {
  # Read the score data to get the required parameters
  score_data = readr::read_tsv(score_path, n_max=1, col_types=readr::cols())
  score_names = c('Threshold 0/1+', 'Threshold 1+/2+', 'Threshold 2+/3+')
  if (!all(score_names %in% names(score_data)))
    stop('"', score_path, '" does not seem to be an H-Score data file.')

  thresholds = score_data[,score_names] %>%
    purrr::map(~{
      # Fix for files using comma as decimal separator
      ifelse(is.character(.x),
             as.numeric(stringr::str_replace(.x, ',', '.')),
                        .x)
             }) %>%
    purrr::flatten_dbl()
  if (is.null(tissue_categories))
    tissue_categories = unique(score_data$`Tissue Category`)
  measure = paste(score_data$`Cell Compartment`[1],
                  score_data$`Stain Component`[1],
                  'Mean', sep=' ')

  compute_h_score(csd, measure, tissue_categories, thresholds, .by)
}

#' Compute H-Score for a single marker aggregated by `.by`
#'
#' Parameters are directly provided.
#'
#' @param csd Cell seg data to use.
#' @param measure The cell seg data column to measure (as a character string)
#' @param tissue_categories A character vector of tissue category names
#'   of interest.
#' @param thresholds Optional three element vector with the threshold values.
#' @param .by Column to aggregate by
#' @return A data frame with one row per Slide ID, showing cell counts and
#'   percents in each bin and the H-Score. The data frame has attributes
#'   measure and thresholds.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_h_score = function(csd, measure, tissue_categories, thresholds,
                           .by='Slide ID') {
  if (!measure %in% names(csd))
    stop("Column not found: ", measure)

  if (length(tissue_categories)==0 ||
      !all(tissue_categories %in% unique(csd$`Tissue Category`)))
    stop('Incorrect tissue categories.')

  if (length(thresholds) != 3)
    stop('Please provide three threshold values.')

  .by = rlang::sym(.by)
  measure = rlang::sym(measure)

  # We only need three columns
  d = csd %>% dplyr::select(!!.by, `Tissue Category`, !!measure) %>%
    dplyr::filter(`Tissue Category` %in% tissue_categories)

  d = d %>% dplyr::mutate(score = dplyr::case_when(
    !!measure < thresholds[1] ~ 0,
    !!measure < thresholds[2] ~ 1,
    !!measure < thresholds[3] ~ 2,
    TRUE ~ 3)) %>%
    dplyr::group_by(!!.by, `Tissue Category`) %>%
    dplyr::summarize(
      `Count of 0+` = sum(score==0),
      `Count of 1+` = sum(score==1),
      `Count of 2+` = sum(score==2),
      `Count of 3+` = sum(score==3),
      Total = dplyr::n()
    ) %>% dplyr::ungroup()

  d = add_tissue_category_totals(d, tissue_categories, .by) %>%
    dplyr::mutate(
      `0+` = round(`Count of 0+`/Total, 3),
      `1+` = round(`Count of 1+`/Total, 3),
      `2+` = round(`Count of 2+`/Total, 3),
      `3+` = round(`Count of 3+`/Total, 3),
      `H-Score` = round(100 * (`1+` + 2*`2+` + 3*`3+`))
    )

  structure(d, measure=measure, thresholds=thresholds)
}
