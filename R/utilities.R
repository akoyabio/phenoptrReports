# Helpers

#' Parse a vector of phenotype names to create selectors for
#' [phenoptr::select_rows()].
#'
#' @param phenos A character vector of phenotype names.
#' @return A named list of phenotype selectors
#' @section Details:
#' Each name must be either a single phenotype name (e.g. CD3+ or CD8-)
#' or two or more names separated by a slash (/). Additionally,
#' a name without a + or - and containing either "Total" or "All" will be
#' interpreted as meaning "All cells".
#' @importFrom magrittr %>%
#' @export
parse_phenotypes = function(phenos) {
  # This does the basic decoding
  # case_when is a bit of a pain...
  dplyr::case_when(
    # Multiple phenotypes become a list
    stringr::str_detect(phenos, '/') ~ str_split(phenos, '/'),

    # Ends with +- and no / is a single phenotype
    stringr::str_detect(phenos, '[+-]$') ~ as.list(phenos),

    # Contains Total or All returns NA which signals "Select All"
    stringr::str_detect(phenos, stringr::regex('Total|All', ignore_case=TRUE)) ~
      rep(NA, length(phenos)) %>% as.list
  ) %>%
    rlang::set_names(phenos) %>%

    # Combinations need to be represented as lists, not vectors
    purrr::map(~{if(length(.x)<=1) .x else as.list(.x)})
}

#' Make a nested data frame with one row per Slide ID and Tissue Category.
#'
#' Nested data is easier to work with than grouped data when the processing
#' is complex.
#' @param csd Cell seg data to use, possibly nested already.
#' @return A nested data frame.
make_nested = function(csd) {
  if (!'Slide ID' %in% names(csd) || !'Tissue Category' %in% names(csd))
    stop('Data frame must have "Slide ID" and "Tissue Category" columns.')

  # If it is already nested, just return it
  if ('data' %in% names(csd) && inherits(csd$data[[1]], 'data.frame'))
    return (csd)

  tidyr::nest(csd, -`Slide ID`, -`Tissue Category`)
}
