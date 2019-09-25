# Helpers

#' Order a data frame by slide ID and tissue category, putting the
#' categories in the given order and the
#' "Total" category in the proper place.
#'
#' @param d A data frame with `.by` and Tissue Category columns
#' @param tissue_categories A vector of category names in the desired order
#' @param .by First column to sort by
#' @return The input, sorted
#' @export
order_by_slide_and_tissue_category =
    function(d, tissue_categories, .by='Slide ID') {
  .by = rlang::sym(.by)

  # Lookup table for ordering tissue categories
  tissue_order = 1:(length(tissue_categories)+2) %>%
    rlang::set_names(c(tissue_categories, 'Total', 'All'))

  d %>%
    dplyr::arrange(!!.by, tissue_order[`Tissue Category`])
}

# Make a nested data frame with one row per Slide ID and
# optionally Tissue Category.
#
# Nested data is easier to work with than grouped data when the processing
# is complex.
# @param csd Cell seg data to use, possibly nested already.
# @param tissue_categories If provided, the result will be filtered
#   and nested by the provided categories.
# @param .by Column to aggregate by
# @return A nested data frame.
make_nested = function(csd, tissue_categories=NULL, .by='Slide ID') {
  # If it is already nested, just return it
  if ('data' %in% names(csd) && inherits(csd$data[[1]], 'data.frame'))
    return (csd)

  if (!rlang::as_string(.by) %in% names(csd))
    stop('Data frame must have "Slide ID" column.')

  .by = rlang::sym(.by)

  # If no tissue categories, just nest by .by
  if (is.null(tissue_categories))
    return(tidyr::nest(csd, data=c(-!!.by)))

  csd = csd %>% dplyr::filter(`Tissue Category` %in% tissue_categories)
  tidyr::nest(csd, data=c(-!!.by, -`Tissue Category`))
}

# Add total rows to a data frame if there are multiple tissue categories.
#
# @param d A data frame with columns for Slide ID, Tissue Category and
#   columns to summarize
# @param tissue_categories The tissue categories of interest, ordered.
# @param .by Column to aggregate by
# @return A data frame with tissue category totals and rows in order.
add_tissue_category_totals = function(d, tissue_categories, .by='Slide ID') {
  if (length(tissue_categories) < 2)
    return(d)

  .by = rlang::sym(.by)
  totals = d %>% dplyr::select(-`Tissue Category`) %>%
    dplyr::group_by(!!.by) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::mutate(`Tissue Category` = 'Total')
  result = dplyr::bind_rows(d, totals)

  result %>% order_by_slide_and_tissue_category(tissue_categories, .by)
}

# Check that all requested phenotypes are defined
# @param params Phenotype names.
# @param phenotypes A named list of phenotype definitions.
check_phenotypes = function(params, phenotypes) {
  if (is.null(params))
      stop('Parameter list must be named.')

  missing_phenotypes = setdiff(params, names(phenotypes))
  if (length(missing_phenotypes) > 0)
    stop("These phenotypes are not defined: ",
         paste(missing_phenotypes, collapse=' ,'), '.')
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

# Check if a function is available in a package
# @param package Name of the package
# @param fun Name of the function
# @return TRUE if the package is installed and contains the function.
function_exists =function(package, fun) {
  requireNamespace(package, quietly=TRUE) &&
    (fun %in% getNamespaceExports(package))
}

# Parse a list of numeric values separated by comma and/or space
# @param s A string containing comma/space separated values.
# @return A (possibly empty) numeric vector with NA values for
# any parsing failures.
parse_comma_space_values = function(s) {
  s %>% stringr::str_trim() %>%
    stringr::str_split('[, ] *') %>%
    purrr::pluck(1) %>%
    purrr::discard(~.x=='') %>%
    purrr::map_dbl(~suppressWarnings(as.numeric(.x)))
}

# Parse phenotypes allowing for NA
# @param ... Phenotypes to be decoded, or a list of same,
# optionally with names.
# @return A named list of phenotype selectors for use with
# `phenoptr::select_rows()`. `NA` values in the input will be
# passed through as `NA` values in the result.
parse_phenotypes_with_na = function(...) {
  phenos = list(...)
  purrr::map(phenos, ~(if(is.na(.x)) NA else
                             phenoptr::parse_phenotypes(.x))) %>%
    purrr::flatten()
}

# Function to insert an Akoya favicon link.
# Call from within `tags$head`.
favicon = function() {
  shiny::tags$link(rel="shortcut icon",
   href="https://www.akoyabio.com/application/files/7715/5959/5805/Asset-1.png")
}
