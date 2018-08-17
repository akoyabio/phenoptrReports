# Helpers

#' Parse a vector of phenotype names.
#'
#' @param ... Phenotypes to be decoded, optionally with names.
#' @return A named list of phenotype selectors for use with
#'   [phenoptr::select_rows()].
#' @section Details:
#' Each phenotype must be either a single phenotype name (e.g. CD3+ or CD8-)
#' or two or more names separated by a slash (/) or comma (,). Additionally,
#' a name without a + or - and containing either "Total" or "All" will be
#' interpreted as meaning "All cells".
#' @importFrom magrittr %>%
#' @export
#' @examples
#' parse_phenotypes("CD3+", "CD3+/CD8-", "Total Cells", Macrophage="CD68+,CD163+")
parse_phenotypes = function(...) {
  phenos = list(...)

  # phenos may have names(pheno) == NULL, if no names were provided
  # If any names were provided, missing names will be ''
  if (is.null(names(phenos))) names(phenos)=phenos else {
    no_names = names(phenos) == ''
    names(phenos)[no_names] = phenos[no_names]
  }

  # This does the basic decoding
  purrr::map(phenos, function(pheno) {
    # Multiple AND phenotypes become a list
    if (stringr::str_detect(pheno, '/')) {
      # Can't have comma and slash
      if (stringr::str_detect(pheno, ','))
        stop(paste("Phenotype selectors may not contain both '/' and '.':", pheno))
      as.list(stringr::str_split(pheno, '/')[[1]])
    }

    # Multiple OR phenotypes become a character vector
    else if (stringr::str_detect(pheno, ',')) stringr::str_split(pheno, ',')[[1]]

    # Ends with +- and no '/' or ',' is a single phenotype
    else if (stringr::str_detect(pheno, '[+-]$')) pheno

    # Contains Total or All returns NA which signals "Select All"
    else if (stringr::str_detect(pheno, stringr::regex('Total|All', ignore_case=TRUE)))
      NA
    else stop(paste("Unrecognized phenotype selector:", pheno))
  }) %>%
    rlang::set_names(names(phenos))
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
