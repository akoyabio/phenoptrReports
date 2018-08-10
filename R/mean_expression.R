
#' Compute mean expression of top-expressing cells for multiple phenotypes
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param param_pairs A named list of pairs of phenotype selectors and column names.
#' @param percentile The percentile cutoff for top-expressing cells. For
#'   example, to measure the top quartile, the percentile is 0.75.
#' @param count The number of top expressing cells to use. Only one of
#'   `percentile` and `count` can be provided.
#' @return A data frame with columns for count and mean for each `param_pair`.
#' @importFrom magrittr %>%
#' @importFrom rlang !! :=
#' @export
compute_mean_expression_many = function(
  csd, param_pairs, percentile=NULL, count=NULL)
{
  purrr::map(names(param_pairs), ~{
    name = .x
    phenotype = param_pairs[[name]][[1]]
    param = param_pairs[[name]][[2]]
    d = compute_mean_expression(csd, phenotype, param, percentile, count)
    count_name = paste(name, 'Count')
    param_what = ifelse(is.null(percentile),
                        paste('Top', count),
                        paste0('>', percentile*100, '%ile'))
    param_name = paste(name, param_what, param)
    d %>% dplyr::rename(!!count_name := count, !!param_name := mean)
  }) %>% dplyr::bind_cols()
}

#' Compute mean expression of top-expressing cells for a single phenotype
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param phenotype A phenotype selector. This will be passed to
#'   \code{\link[phenoptr]{select_rows}}.
#' @param param The parameter (column) to report, as a string.
#' @param percentile The percentile cutoff for top-expressing cells. For
#'   example, to measure the top quartile, the percentile is 0.75.
#' @param count The number of top expressing cells to use. Only one of
#'   `percentile` and `count` can be provided.
#' @return A data frame with columns for count and mean.
#' @importFrom magrittr %>%
#' @export
compute_mean_expression = function(
  csd, phenotype, param, percentile=NULL, count=NULL)
{
  # Parameter checking
  stopifnot(is.data.frame(csd))

  if (!param %in% names(csd))
    stop(paste0("The provided data does not have a '", param, "' column."))

  if (is.null(percentile) == is.null(count))
    stop("Please specify either percentile or count, and not both")

  if (!is.null(percentile) && (percentile<0 || percentile > 1))
    stop("percentile must be in the range 0 - 1.")

  if (!is.null(count) && count < 1)
    stop("count must be >= 1.")

  # Select just the cells and column of interest
  d = csd[phenoptr::select_rows(csd, phenotype),][[param]]

  if (!is.null(percentile)) {
    cutoff = stats::quantile(d, percentile, na.rm=TRUE)
    m = mean(d[d>=cutoff], na.rm=TRUE)
  } else {
    # Top n
    m = mean(d[dplyr::min_rank(dplyr::desc(d)) <= count], na.rm=TRUE)
  }

  tibble::data_frame(count=length(d), mean=m)
}
