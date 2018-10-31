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
#' and create summary reports
#'
#' Calls [merge_and_split_cell_seg_data] to merge the supplied
#' cell seg data files. Additionally calls [write_summary_report] to
#' create a summary report for each source file and the merged data.
#'
#' Writes the merged data to `Consolidated_data.txt` in the output
#' directory.
#'
#' @param csd_files A list or vector of paths to cell seg data files.
#' @param output_dir Path to a directory where the results will be saved.
#' @param update_progress Callback function which is called with progress.
#' @return A single data frame containing merged data and columns for each
#'   single phenotype, invisibly.
#' @family merge functions
#' @importFrom magrittr %>%
#' @export
merge_and_summarize_cell_seg_data = function(csd_files, output_dir,
                                             update_progress=NULL) {
  # Make some names, these will be for files and headers
  names = make_unique_names(csd_files)

  if (!dir.exists(output_dir))
    stopifnot(dir.create(output_dir, recursive=TRUE))

  # Make a progress function if we don't have one so we don't have to
  # check every time
  if (!is.function(update_progress))
    update_progress = function(...) {}

  # Read all the files so we read each one only once
  update_progress(detail='Reading source files.')
  data = purrr::map(csd_files, phenoptr::read_cell_seg_data)

  # Summary reports for the raw data
  purrr::walk2(names, data, function(n, d) {
    update_progress(detail=paste0('Writing report for "', n, '".'))
    out_path = file.path(output_dir, paste0(n, '.html'))
    write_summary_report(csd=d, output_path=out_path, dataset_name=n)
  })

  # Merge and write the consolidated data
  update_progress(detail='Merging...')
  csd = merge_and_split_cell_seg_data(data=data)
  readr::write_tsv(csd, file.path(output_dir, 'Consolidated_data.txt'))

  # And the report
  update_progress(detail='Writing report for consolidated data.')
  write_summary_report(csd=csd,
    output_path=file.path(output_dir, 'Consolidated_data.html'),
    dataset_name='Consolidated data')

  invisible(csd)
}

#' Merge cell seg data files from parallel projects
#'
#' Merge several cell seg data files, each with its own `Phenotype` column,
#' into a single file with separate columns for each phenotype.
#'
#' The
#' individual files must all have exactly the same `Sample Name` and `Cell ID`
#' columns. [split_phenotypes] is called to split the `Phenotype` columns.
#'
#' @param csd_files A list or vector of paths to cell seg data files.
#' @param data A list of data tables from read_cell_seg_data. Exactly one
#'   of `files` or `data` should be provided.
#' @return A single data frame containing merged data and columns for each
#'   single phenotype.
#' @family merge functions
#' @importFrom magrittr %>%
#' @export
merge_and_split_cell_seg_data = function(csd_files=NULL, data=NULL) {
  if (is.null(csd_files) == is.null(data))
    stop("Provide either 'csd_files' or 'data' but not both.")

  if (!is.null(csd_files)) {
    csd_files = unlist(csd_files)
    if (!is.character(csd_files) || !all(purrr::map_lgl(csd_files, file.exists)))
      stop('Please pass a list of paths to existing cell seg data files.')

    data = purrr::map(csd_files, phenoptr::read_cell_seg_data)
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
    csd = dplyr::inner_join(csd, csd2, by=c('Sample Name', 'Cell ID'))
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
  csd %>% dplyr::select(-Phenotype, -dplyr::contains('Confidence')) %>%
    dplyr::bind_cols(new_columns)
}

#' Make unique names from a list of paths to cell seg data files
#'
#' If the files have unique names, returns the base names without extension.
#' If the files do not have unique names, returns the directory name
#' plus the file name without extension.
#' If those are not unique, appends a sequence number to each name.
#' @param csd_files A list or vector of paths to cell seg data files.
#' @return A vector of names.
make_unique_names = function(csd_files) {
  # Get the base names sans extension
  names = csd_files %>% purrr::map_chr(basename) %>%
    stringr::str_remove('\\.txt')

  # If they are unique, we are done
  if (length(unique(names)) == length(names))
    return(names)

  # Try prefixing the directory names
  dir_names = csd_files %>% purrr::map_chr(~basename(dirname(.x)))
  names = paste(dir_names, names, sep='_')
  if (length(unique(names)) == length(names))
    return(names)

  # Suffix sequence numbers
  paste(names, seq_along(names), sep='_')
}
