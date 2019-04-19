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
  tissue_order = 1:(length(tissue_categories)+2) %>%
    rlang::set_names(c(tissue_categories, 'Total', 'All'))

  d %>%
    dplyr::arrange(`Slide ID`, tissue_order[`Tissue Category`])
}

#' Make a nested data frame with one row per Slide ID and
#' optionally Tissue Category.
#'
#' Nested data is easier to work with than grouped data when the processing
#' is complex.
#' @param csd Cell seg data to use, possibly nested already.
#' @param tissue_categories If provided, the result will be filtered
#'   and nested by the provided categories.
#' @return A nested data frame.
make_nested = function(csd, tissue_categories=NULL) {
  # If it is already nested, just return it
  if ('data' %in% names(csd) && inherits(csd$data[[1]], 'data.frame'))
    return (csd)

  if (!'Slide ID' %in% names(csd))
    stop('Data frame must have "Slide ID" column.')

  # If no tissue categories, just nest by Slide ID
  if (is.null(tissue_categories))
    return(tidyr::nest(csd, -`Slide ID`))

  csd = csd %>% dplyr::filter(`Tissue Category` %in% tissue_categories)
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

  totals = d %>% dplyr::select(-`Tissue Category`) %>%
    dplyr::group_by(`Slide ID`) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::mutate(`Tissue Category` = 'Total')
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

#' Check that a user-specified phenotype definition
#' can be formed from available phenotypes.
#' @param pheno Text description of a phenotype,
#' for `phenoptr::parse_phenotypes`.
#' @param available A character vector of available phenotypes
#' @return An error message or empty string
#' @export
validate_phenotype_definitions = function(pheno, available) {
  if (is.null(pheno) || pheno==''
      || stringr::str_detect(pheno, 'Total|All'))
    return('')

  phenos = stringr::str_split(pheno, '[,/]')[[1]] %>%
    stringr::str_trim()

  if (!all(stringr::str_detect(phenos, '[+-]$')))
    return('Phenotype definitions must end with + or -.')

  phenos = stringr::str_remove(phenos, '[+-]$')
  missing = !phenos %in% available
  if (any(missing))
    return(paste0('Unknown phenotype(s): ', paste(phenos[missing], sep=', ')))

  return('')
}

#' Cross-platform choose directory function.
#' @param caption Caption for the choose directory dialog
#' @param default Starting directory
#' @return The path to the selected directory, or NA if the user canceled.
#' @export
# Inspired by https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
choose_directory = function(caption = 'Select folder', default='') {
  if (function_exists('utils', 'choose.dir')) {
    utils::choose.dir(caption = caption, default=default)
  } else if (function_exists('rstudioapi', 'isAvailable') &&
             rstudioapi::isAvailable() &&
             rstudioapi::getVersion() > '1.1.287') {
    rstudioapi::selectDirectory(caption = caption, path=default)
  } else if (function_exists('tcltk', 'tk_choose.dir')) {
    tcltk::tk_choose.dir(caption = caption, default=default)
  } else stop('No directory chooser available.')
}

#' Cross-platform choose files function
#' @param caption Caption for the choose directory dialog
#' @param default Starting directory
#' @param multi Allow multiple files to be selected
#' @param filters A two-column matrix of filename filters, or a two-element
#' vector containing a single filter.
#' @return The path to the selected file(s), or NA if the user canceled.
#' @export
choose_files = function(caption='Select files', default='',
                        multi=TRUE, filters=NULL) {
  if (!is.null(filters) && is.vector(filters))
    filters = matrix(filters, byrow=TRUE, ncol=2)

  # Use Windows-only choose.files() if available
  if (function_exists('utils', 'choose.files')) {
    files = utils::choose.files(caption = caption, default=default,
                        multi=multi, filters=filters)
    return(files)
  }

  # If multi==FALSE, rstudioapi::selectFile() is the next best option.
  # If multi==TRUE, use tcltk::tk_choose.files if it is available,
  # it supports multiple selection; otherwise use
  # selectFile() for single-file-only selection.

  # tk_choose.files fails on RStudio Server even though the function exists.
  # I think the fail is because X11 is not available.
  # This ugly thing tries to open a TK window to find out if TK is
  # really available.
  tk_avail = function_exists('tcltk', 'tk_choose.files') &&
    class(suppressMessages(try({
      tt  <- tcltk::tktoplevel();
      tcltk::tkdestroy(tt)
    }, silent = TRUE))) != 'try-error'

  if ((!multi || !tk_avail) &&
             function_exists('rstudioapi', 'isAvailable') &&
             rstudioapi::isAvailable() &&
             rstudioapi::getVersion() > '1.1.287') {
    rstudioapi::selectFile(caption = caption, path=default,
                           filter=filters[nrow(filters),1])
  } else if (tk_avail) {
    tcltk::tk_choose.files(caption = caption, default=default,
                         multi=multi, filters=filters)
  } else stop('No file chooser available.')
}

#' Check if a function is available in a package
#' @param package Name of the package
#' @param fun Name of the function
#' @return TRUE if the package is installed and contains the function.
function_exists =function(package, fun) {
  requireNamespace(package) && (fun %in% getNamespaceExports(package))
}
