# Functions to merge 'classic' cell seg data files and convert to
# a phenotype per marker form.

# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "Cell ID",
  "Sample Name",
  "Phenotype",
  "Confidence"
))

#' Merge several cell seg data files, each with their own `Phenotype` column,
#' into a single file with separate columns for each phenotype.
#'
#' @param files A list or vector of file paths
#' @return A single data frame containing merged data and columns for each
#'   phenotype.
#' @importFrom magrittr %>%
#' @export
merge_and_split_cell_seg_data = function(files) {
  files = unlist(files)
  if (!is.character(files) || !all(purrr::map_lgl(files, file.exists)))
    stop('Please pass a list of paths to existing cell seg data files.')

  csd = phenoptr::read_cell_seg_data(files[1]) %>% split_phenotypes()

  for (file in files[-1]) {
    csd2 = phenoptr::read_cell_seg_data(file) %>%
      split_phenotypes()%>%
      dplyr::select(`Sample Name`, `Cell ID`, dplyr::starts_with('Phenotype '))
    if (nrow(csd2) != nrow(csd))
      stop(paste0('Number of rows in "', files[1], '" does not match "', file, '".'))
    csd = dplyr::left_join(csd, csd2 )
  }

  csd
}

#' Split a phenotype column containing multiple positivities
#' into multiple columns, one for each single phenotype.
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
    positive = .x
    negative = stringr::str_replace(positive, '\\+$', '-')
    result = rep(negative, nrow(csd))
    result[blanks] = ''
    result[stringr::str_detect(csd$Phenotype, stringr::fixed(positive))] = positive
    result
  })

  new_names = paste('Phenotype', stringr::str_remove(positives, '\\+$'))
  new_columns = new_columns %>% rlang::set_names(new_names)

  csd %>% dplyr::select(-Phenotype, -Confidence) %>%
    dplyr::bind_cols(new_columns)
}
