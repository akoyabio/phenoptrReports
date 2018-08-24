# Functions to merge 'classic' cell seg data files and convert to
# a phenotype per marker form.

# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "Cell ID",
  "Sample Name",
  "Phenotype",
  "Confidence"
))

#' Merge cell seg data files from parallel projects
#'
#' Merge several cell seg data files, each with its own `Phenotype` column,
#' into a single file with separate columns for each phenotype.
#'
#' The
#' individual files must all have exactly the same `Sample Name` and `Cell ID`
#' columns. [split_phenotypes] is called to split the `Phenotype` columns.
#'
#' @param files A list or vector of paths to cell seg data files
#' @param data A list of data tables from read_cell_seg_data. Exactly one
#'   of `files` or `data` should be provided.
#' @return A single data frame containing merged data and columns for each
#'   single phenotype.
#' @importFrom magrittr %>%
#' @export
merge_and_split_cell_seg_data = function(files=NULL, data=NULL) {
  if (is.null(files) == is.null(data))
    stop("Provide either 'files' or 'data' but not both.")

  if (!is.null(files)) {
    files = unlist(files)
    if (!is.character(files) || !all(purrr::map_lgl(files, file.exists)))
      stop('Please pass a list of paths to existing cell seg data files.')

    data = purrr::map(files, phenoptr::read_cell_seg_data)
  } else {
    if (!all(purrr::map_lgl(data, ~inherits(., 'data.frame'))))
      stop("Please pass a list of data frames as 'data'.")
  }

  # Read the first file, we will use it as the basis for the result
  csd = data[[1]] %>% split_phenotypes()

  # Read subsequent files, split phenotypes, join with the first file.
  for (csd2 in data[-1]) {
    csd2 = csd2 %>%
      split_phenotypes() %>%
      dplyr::select(`Sample Name`, `Cell ID`, dplyr::starts_with('Phenotype '))

    if (nrow(csd2) != nrow(csd))
      stop(paste0('Number of rows in data frames do not match.'))
    csd = dplyr::inner_join(csd, csd2 )
  }

  if (nrow(csd) != nrow(data[[1]]))
    stop('Rows of data frames do not match.')

  csd
}

#' Split a phenotype column
#'
#' A column containing multiple phenotypes is split
#' into multiple columns, one for each single phenotype.
#'
#' Multiple phenotypes in the original column must be separated with "/".
#' The names of positive phenotypes must end in "+".
#'
#' @param csd Cell seg data to use.
#' @return A new data frame with the `Phenotype` column replaced with
#'   individual columns per phenotype and the `Confidence` column removed.
#' @importFrom magrittr %>%
#' @export
split_phenotypes = function(csd) {
  if (!'Phenotype' %in% names(csd))
    stop('Cell seg data does not have a Phenotype column.')

  # Look for positive phenotypes
  phenotypes = unique(csd$Phenotype) %>%
    stringr::str_split('/') %>%
    purrr::flatten_chr()
  positives = phenotypes[endsWith(phenotypes, '+')] %>% unique()

  if (length(positives) == 0)
    stop('No positive phenotypes found.')

  # If there is no phenotype in the original, leave the new ones blank as well
  blanks = csd$Phenotype == ''

  # Make a new column for each positive phenotype
  new_columns = purrr::map(positives, ~{
    # Positive and negative values for the new column
    positive = .x
    negative = stringr::str_replace(positive, '\\+$', '-')

    # Start out all negative or blank
    result = rep(negative, nrow(csd))
    result[blanks] = ''

    # Fill in the positive value anywhere it appears in the original
    result[stringr::str_detect(csd$Phenotype, stringr::fixed(positive))] = positive
    result
  })

  # Make names for the new columns
  new_names = paste('Phenotype', stringr::str_remove(positives, '\\+$'))
  new_columns = new_columns %>% rlang::set_names(new_names)

  # Build a new data frame
  csd %>% dplyr::select(-Phenotype, -Confidence) %>%
    dplyr::bind_cols(new_columns)
}
