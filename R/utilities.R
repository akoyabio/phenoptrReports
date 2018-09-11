# Helpers

#' Order a data frame by slide ID and tissue category, putting the
#' categories in the given order and the
#' "Total" category in the proper place.
#'
#' @param d A data frame with Slide ID and Tissue Category columns
#' @param tissue_categories A vector of category names in the desired order
#' @return The input, sorted
#' @export
order_by_slide_and_tissue_category = function(d, tissue_categories) {
  # Lookup table for ordering tissue categories
  tissue_order = 1:(length(tissue_categories)+1) %>%
    rlang::set_names(c(tissue_categories, 'Total'))

  d %>%
    dplyr::arrange(`Slide ID`, tissue_order[`Tissue Category`])
}

#' Make a nested data frame with one row per Slide ID and Tissue Category.
#'
#' Nested data is easier to work with than grouped data when the processing
#' is complex.
#' @param csd Cell seg data to use, possibly nested already.
#' @return A nested data frame.
make_nested = function(csd) {
  # If it is already nested, just return it
  if ('data' %in% names(csd) && inherits(csd$data[[1]], 'data.frame'))
    return (csd)

  if (!'Slide ID' %in% names(csd) || !'Tissue Category' %in% names(csd))
    stop('Data frame must have "Slide ID" and "Tissue Category" columns.')

  tidyr::nest(csd, -`Slide ID`, -`Tissue Category`)
}

#' Add total rows to a data frame if there are multiple tissue categories.
#'
#' @param d A data frame with columns for Slide ID, Tissue Category and
#'   columns to summarize
#' @param tissue_categories The tissue categories of interest, ordered.
#' @return A data frame with tissue category totals and rows in order.
add_tissue_category_totals = function(d, tissue_categories) {
  if (length(tissue_categories) < 2)
    return(d)

  totals = d %>% dplyr::group_by(`Slide ID`) %>%
    dplyr::summarize_at(dplyr::vars(-(1:2)), sum)
  totals$`Tissue Category` = 'Total'
  result = dplyr::bind_rows(d, totals)

  result %>% order_by_slide_and_tissue_category(tissue_categories)
}

#' Check that all requested phenotypes are defined
#' @param params Phenotype names.
#' @param phenotypes A named list of phenotype definitions.
check_phenotypes = function(params, phenotypes) {
  if (is.null(params))
      stop('Parameter list must be named.')

  missing_phenotypes = setdiff(params, names(phenotypes))
  if (length(missing_phenotypes) > 0)
    stop("These phenotypes are not defined: ",
         paste(missing_phenotypes, collapse=' ,'), '.')
}
