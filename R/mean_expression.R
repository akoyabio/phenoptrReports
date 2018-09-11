# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "data"
))

#' Compute mean expression of top-expressing cells for multiple phenotypes
#'
#' For each given combination of phenotype and expression parameter,
#' find the cells with the highest expression within the phenotype.
#' Report the mean expression of the high-expressing cells.
#'
#' High expression may be specified by a percentile cutoff or a count of cells.
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest. It may already be nested by
#'   `Slide ID` and `Tissue Category`.
#' @param phenotypes A named list of phenotype selectors
#'  (see [phenoptr::parse_phenotypes]).
#' @param params A named list matching phenotype names to
#'   expression column names.
#' @param tissue_categories Optional vector of tissue category names to include.
#' @param percentile The percentile cutoff for top-expressing cells. For
#'   example, to measure the top quartile, the percentile is 0.75.
#' @param count The number of top expressing cells to use. Only one of
#'   `percentile` and `count` can be provided. If both are omitted,
#'   all cells matching the phenotype are used and the result is the
#'   overall mean expression.
#' @return A data frame with columns for count and mean for each `param_pair`.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @importFrom rlang !! :=
#' @export
compute_mean_expression_many = function(
  csd, phenotypes, params, tissue_categories=NULL, percentile=NULL, count=NULL)
{
  check_phenotypes(names(params), phenotypes)

  if (!is.null(tissue_categories)) {
    csd = csd %>% dplyr::filter(`Tissue Category` %in% tissue_categories)
  }

  csd = make_nested(csd)

  # Function to compute all expressions for a single nested data frame
  compute_means = function(d) {
    purrr::map_dfc(names(params), ~{
      # Phenotype name, phenotype selector, expression parameter name
      name = .x
      phenotype = phenotypes[[name]]
      param = params[[name]]

      # Compute a single expression value
      d = compute_mean_expression(d, phenotype, param, percentile, count)

      # Make nice column names so we can bind them all together
      count_name = paste(name, 'Count')
      param_what = dplyr::case_when(!is.null(percentile) ~
                                      paste0(' >', percentile*100, '%ile'),
                          !is.null(count) ~paste(' Top', count),
                          TRUE ~ '')
      param_name = paste0(name, param_what, ' ', param)
      d %>% dplyr::rename(!!count_name := count, !!param_name := mean)
    })
  }

  # Actually do the computation for each nested data frame
  result = csd %>% dplyr::mutate(means = purrr::map(data, compute_means)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()

  # Fix up the row order so Slide ID order is preserved and
  # tissue categories are in the order given
  if (!is.null(tissue_categories)) {
    slides = unique(result$`Slide ID`)
    slide_order = seq_along(slides) %>% rlang::set_names(slides)
    tissue_order = seq_along(tissue_categories) %>%
      rlang::set_names(tissue_categories)
    result = result %>%
      dplyr::arrange(slide_order[`Slide ID`],
                     tissue_order[`Tissue Category`]) %>%
      dplyr::select(`Slide ID`, `Tissue Category`, dplyr::everything())
  }

  result
}

#' Compute mean expression of top-expressing cells for a single phenotype
#'
#' Find the cells with the highest expression of the given parameter
#' within the given phenotype.
#' Report the mean expression of the high-expressing cells.
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param phenotype A phenotype selector. This will be passed to
#'   [phenoptr::select_rows].
#' @param param The parameter (column) to report, as a string.
#' @param percentile The percentile cutoff for top-expressing cells. For
#'   example, to measure the top quartile, the percentile is 0.75.
#' @param count The number of top expressing cells to use. Only one of
#'   `percentile` and `count` can be provided. If both are omitted,
#'   the mean expression of all cells is returned.
#' @return A data frame with columns for count and mean.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_mean_expression = function(
  csd, phenotype, param, percentile=NULL, count=NULL)
{
  # Parameter checking
  stopifnot(is.data.frame(csd))

  if (!param %in% names(csd))
    stop(paste0("The provided data does not have a '", param, "' column."))

  if (!is.null(percentile) && !is.null(count))
    stop("Please specify only one of percentile or count, not both")

  if (!is.null(percentile) && (percentile<0 || percentile > 1))
    stop("percentile must be in the range 0 - 1.")

  if (!is.null(count) && count < 1)
    stop("count must be >= 1.")

  # Select just the cells and column of interest
  d = csd[phenoptr::select_rows(csd, phenotype),][[param]]

  if (!is.null(percentile)) {
    cutoff = stats::quantile(d, percentile, na.rm=TRUE)
    m = mean(d[d>=cutoff], na.rm=TRUE)
  } else if (!is.null(count)) {
    # Top n
    m = mean(d[dplyr::min_rank(dplyr::desc(d)) <= count], na.rm=TRUE)
  } else {
    # Mean expression of all cells
    m = mean(d, na.rm=TRUE)
  }

  # Report NA rather than NaN
  m[is.nan(m)] = NA

  tibble::data_frame(count=length(d), mean=m)
}
