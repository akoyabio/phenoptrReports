# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  '1+', '2+', '3+',
  'Count of 0+', 'Count of 1+', 'Count of 2+', 'Count of 3+',
  'Total', 'score', 'positivity'
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
                                   tissue_categories=NULL) {
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
  csd %>%
    dplyr::mutate(positivity = purrr::map(data, compute_data_frame)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(positivity)
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
  d = csd[phenoptr::select_rows(csd, phenotype), ]
  count = nrow(d) # Count of all positive

  d = d[phenoptr::select_rows(d, positivity), ]
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
#' @param phenotype Optional phenotype to subset `csd`.
#' @return A data frame with one row per Slide ID, showing cell counts and
#'   percents in each bin and the H-Score. See [compute_h_score].
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_h_score_from_score_data = function(csd, score_path,
                                           tissue_categories=NULL,
                                           .by='Slide ID',
                                           phenotype=NULL) {
  # Read the score data to get the required parameters
  score_data = readr::read_tsv(score_path, n_max=1, col_types=readr::cols())
  score_names = c('Threshold 0/1+', 'Threshold 1+/2+', 'Threshold 2+/3+')
  if (!all(score_names %in% names(score_data)))
    stop('"', score_path, '" does not seem to be an H-Score data file.')

  thresholds = score_data[, score_names] %>%
    purrr::map(~{
      # Fix for files using comma as decimal separator
      ifelse(is.character(.x),
             as.numeric(stringr::str_replace(.x, ',', '.')),
                        .x)
             }) %>%
    purrr::flatten_dbl()

  if (is.null(tissue_categories))
    tissue_categories = unique(score_data$`Tissue Category`)

  # Remember the full combinations of .by and tissue_categories
  # so we can reconstruct missing combinations
  tc = tissue_categories
  if (length(tc)>1) tc = c(tc, 'Total')
  full_combos = tidyr::expand_grid(!!.by:=sort(unique(csd[[.by]])),
                                   `Tissue Category`=tc)

  # Now subset to the specified phenotype
  if (!is.null(phenotype)) {
    csd = csd[phenoptr::select_rows(csd, phenotype), ]
  }

  measure = paste(score_data$`Cell Compartment`[1],
                  score_data$`Stain Component`[1],
                  'Mean', sep=' ')

  result = compute_h_score(csd, measure, tissue_categories, thresholds, .by)

  # H-Scores for rare phenotypes missing any counts are populated from scratch,
  # corresponding NA values are later reported as "#N/A" values in Excel
  if (nrow(result) != nrow(full_combos)) {
    # Add in missing combinations
    fill = rep(NA, 5) %>% rlang::set_names(names(result)[3:7]) %>% as.list()
    result = full_combos %>%
      dplyr::left_join(result) %>%
      tidyr::replace_na(replace = fill) %>%
      structure(measure=attr(result, 'measure'),
                thresholds=attr(result, 'thresholds'))
  }

  result
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

  if (length(thresholds) != 3)
    stop('Please provide three threshold values.')

  .by = rlang::sym(.by)
  measure = rlang::sym(measure)

  # We only need three columns
  d = csd %>%
    dplyr::select(!!.by, `Tissue Category`, !!measure) %>%
    dplyr::filter(`Tissue Category` %in% tissue_categories)

  # Score each cell compartment and summarize
  d = d %>%
    dplyr::mutate(score = dplyr::case_when(
    # score numeric values
    !!measure >= thresholds[3] ~ 3,
    !!measure >= thresholds[2] ~ 2,
    !!measure >= thresholds[1] ~ 1,
    !!measure < thresholds[1] ~ 0),
    # ignore non-numeric values ("#N/A")
    #   inForm is scoring missing values as `0+`
    #   phenoptrReports was scoring missing values as `3+`
    .default = NA) %>%
    dplyr::group_by(!!.by, `Tissue Category`) %>%
    dplyr::summarize(
      # summarize only numeric values
      `Count of 0+` = sum(score==0, na.rm = TRUE),
      `Count of 1+` = sum(score==1, na.rm = TRUE),
      `Count of 2+` = sum(score==2, na.rm = TRUE),
      `Count of 3+` = sum(score==3, na.rm = TRUE),
      # total percentages include numeric values only
      #   inForm reports `Number of Cells`, not measurements, across all tissues
      Total = `Count of 0+` + `Count of 1+` + `Count of 2+` + `Count of 3+`
    ) %>%
    dplyr::ungroup()

  # Add missing tissue categories
  fill = rep(0, 5) %>% rlang::set_names(names(d)[3:7]) %>% as.list()
  d = d %>%
    tidyr::complete(!!.by, `Tissue Category`, fill=fill)

  d = add_tissue_category_totals(d, tissue_categories, .by) %>%
    dplyr::mutate(
      `0+` = round(`Count of 0+`/Total, 3),
      `1+` = round(`Count of 1+`/Total, 3),
      `2+` = round(`Count of 2+`/Total, 3),
      `3+` = round(`Count of 3+`/Total, 3),
      `H-Score` = round(100 * (`1+` + 2*`2+` + 3*`3+`))
    )

  # Replace NaN with NA
  d = d %>% dplyr::mutate_if(is.numeric, ~ifelse(is.nan(.), NA, .))

  structure(d, measure=measure, thresholds=thresholds)
}
