# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "data", "name", "means"
))

#' Compute mean expression of cells for multiple phenotypes and markers.
#'
#' For each given combination of phenotype and expression parameter,
#' report the mean expression of the given marker in the specified cells.
#'
#' This is a very flexible function. If `percentile` and `count` are both
#' omitted, it will compute the mean expression for all cells in the
#' given phenotype. If either `percentile` or `count` is given, the mean
#' expression of the highest expressing cells is computed. If a negative
#' `percentile` is given, the lowest percentile is used; for example
#' `percentile=-.1` would give the expression of the lowest-expressing
#' decile.
#'
#' By default, this function aggregates by `Slide ID` and `Tissue Category`.
#' To compute mean expression by a different aggregate, pass a column name in
#' the `.by` parameter.
#'
#' To aggregate over all cells, include `"Total Cells"=NA` as one of the
#' phenotypes.
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest. It may already be nested by
#'   !!.by and `Tissue Category`.
#' @param phenotypes A named list of phenotype selectors
#'  (see [phenoptr::parse_phenotypes]).
#' @param params A named list matching phenotype names to
#'   expression column names.
#' @param tissue_categories Optional vector of tissue category names to include.
#' @param percentile The percentile cutoff for top-expressing cells. For
#'   example, to measure the top quartile, the percentile is 0.75. Negative
#'   numbers will use low-expressing cells; to measure the bottom decile,
#'   use a percentile of -0.1.
#' @param count The number of top expressing cells to use. Only one of
#'   `percentile` and `count` can be provided. If both are omitted,
#'   all cells matching the phenotype are used and the result is the
#'   overall mean expression.
#' @param .by Column to aggregate by
#' @return A data frame with a column for mean for each `param_pair`.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
#' @export
compute_mean_expression_many = function(csd, phenotypes, params,
                                        tissue_categories=NULL, percentile=NULL,
                                        count=NULL, .by='Slide ID') {
  check_phenotypes(names(params), phenotypes)
  .by = rlang::sym(.by)

  csd = make_nested(csd, tissue_categories, .by)


  # If we are grouping by .by and Tissue Category, and have more than
  # one TC, we will add Total rows later. Figure out now whether we will
  # need to do that.
  add_tc_totals = (length(tissue_categories)>1
    && length(names(csd)) == 3
    && rlang::as_string(.by) %in% names(csd)
    && 'Tissue Category' %in% names(csd))

  # Figure out what we will call the eventual expression columns.
  # Avoid `-NULL` in the case_when...
  safe_percentile = ifelse(is.null(percentile), 0, percentile)

  param_what = dplyr::case_when(
    !is.null(percentile) && percentile > 0
      ~ paste0(' >= ', safe_percentile*100, '%ile'),
    !is.null(percentile) && percentile < 0
      ~ paste0(' <= ', -safe_percentile*100, '%ile'),
    !is.null(count) ~ paste(' Top', count),
    TRUE ~ '')

  param_names = paste0(names(params), param_what, ' ', params)

  # Don't allow duplicate measures, it breaks downstream and is not helpful
  dups = duplicated(param_names)
  if (any(dups)) {
    warning('Removing duplicate expression parameters: ',
            paste(param_names[dups], collapse=', '))
    params = params[!dups]
    param_names = param_names[!dups]
  }

  # Function to compute all expressions for a single nested data frame
  # Returns a data frame with columns for count, mean, and name
  compute_means = function(d) {
    purrr::map_dfr(seq_along(params), function(ix) {
      # Phenotype name, phenotype selector, expression parameter name
      # Use numeric indexing on params to allow multiple markers per phenotype
      name = names(params)[ix]
      phenotype = phenotypes[[name]]
      param = params[[ix]]

      # Compute a single expression value
      d = compute_mean_expression(d, phenotype, param, percentile, count)

      d$name = param_names[ix]
      d
    })
  }

  # Actually do the computation for each nested data frame
  result = csd %>%
    dplyr::mutate(means = purrr::map(data, compute_means)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(means)

  if (!add_tc_totals) {
    # Names of the non-expression columns
    cols = utils::head(names(result), -3)
    result = result %>% dplyr::select(-count) %>% tidyr::spread(name, mean)

    # Fix up column order to match the parameter order
    return(result[, c(cols, param_names)])
  }

  # Add "All" rows by .by, fill in missing tissue categories,
  # spread to columns, and fix column order
  # to match params
  result %>%
    dplyr::group_by(!!.by, name) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=purrr::map(data, function(d) {
      total = tibble::tibble(count=sum(d$count, na.rm=TRUE),
                  mean=sum(d$count*d$mean, na.rm=TRUE)/sum(d$count, na.rm=TRUE))
      total$`Tissue Category` = 'All'
      dplyr::bind_rows(d, total)
    })) %>%
    tidyr::unnest(data) %>%
    dplyr::select(-count) %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!.by, `Tissue Category`, name) %>%
    tidyr::spread(name, mean) %>%
    order_by_slide_and_tissue_category(tissue_categories, .by) %>%
    dplyr::select(!!.by, `Tissue Category`, !!!param_names)
}

#' Compute mean expression of cells for a single phenotype and marker.
#'
#' Find the cells with the highest (or lowest) expression of the given parameter
#' within the given phenotype.
#' Report the mean expression of the high-expressing cells.
#'
#' @param csd Cell seg data to use. This should already have been filtered
#'   for the slides or fields of interest.
#' @param phenotype A phenotype selector. This will be passed to
#'   [phenoptr::select_rows].
#' @param param The parameter (column) to report, as a string.
#' @param percentile The percentile cutoff for top-expressing cells. For
#'   example, to measure the top quartile, the percentile is 0.75. Negative
#'   numbers will use low-expressing cells; to measure the bottom decile,
#'   use a percentile of -0.1.
#' @param count The number of top expressing cells to use. Only one of
#'   `percentile` and `count` can be provided. If both are omitted,
#'   the mean expression of all cells is returned.
#' @return A data frame with columns for count and mean.
#' @family aggregation functions
#' @importFrom magrittr %>%
#' @export
compute_mean_expression = function(csd, phenotype, param,
                                   percentile=NULL, count=NULL) {
  # Parameter checking
  stopifnot(is.data.frame(csd))

  if (!param %in% names(csd))
    stop(paste0("The provided data does not have a '", param, "' column."))

  if (!is.null(percentile) && !is.null(count))
    stop("Please specify only one of percentile or count, not both")

  if (!is.null(percentile)
      && (percentile==0 || percentile< -1 || percentile > 1))
    stop("percentile must be non-zero and in the range -1 to 1.")

  if (!is.null(count) && count < 1)
    stop("count must be >= 1.")

  # Select just the cells and column of interest
  d = csd[phenoptr::select_rows(csd, phenotype), ][[param]]

  if (!is.null(percentile)) {
    # Top or bottom percentile
    cutoff = stats::quantile(d, abs(percentile), na.rm=TRUE)
    if (percentile > 0) {
      m = mean(d[d>=cutoff], na.rm=TRUE)
    } else {
      m = mean(d[d<=cutoff], na.rm=TRUE)
    }
  } else if (!is.null(count)) {
    # Top n
    m = mean(d[dplyr::min_rank(dplyr::desc(d)) <= count], na.rm=TRUE)
  } else {
    # Mean expression of all cells
    m = mean(d, na.rm=TRUE)
  }

  # Report NA rather than NaN
  m[is.nan(m)] = NA

  tibble::tibble(count=length(d), mean=m)
}
