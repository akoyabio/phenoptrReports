# Functions to consolidate 'classic' cell seg data files and convert to
# a phenotype per marker form.

# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c(
  "Cell ID",
  "Sample Name",
  "Phenotype",
  "Confidence"
))

#' Consolidate cell seg data files from parallel projects
#' and create summary reports
#'
#' Consolidate several cell seg data files,
#' each with its own `Phenotype` column,
#' into a single file with separate columns for each phenotype.
#'
#' Create a summary report for each source file and the consolidated data.
#'
#' Write the consolidated data to `Consolidated_data.txt` in the output
#' directory.
#'
#' The
#' individual files must all have exactly the same
#' `Sample Name` or `Annotation ID` and `Cell ID`
#' columns. [split_phenotypes] is called to split the `Phenotype` columns.
#'
#' @param csd_files A list or vector of paths to cell seg data files.
#' @param output_dir Path to a directory where the results will be saved.
#' @param update_progress Callback function which is called with progress.
#' @param col_select Column selection for [phenoptr::read_cell_seg_data()]
#' @return A single data frame containing consolidated data and columns for each
#'   single phenotype, invisibly.
#' @importFrom magrittr %>%
#' @export
consolidate_and_summarize_cell_seg_data = function(csd_files, output_dir,
                                             update_progress=NULL,
                                             col_select=NULL) {
  if (!dir.exists(output_dir))
    stopifnot(dir.create(output_dir, recursive=TRUE))

  # Make a progress function if we don't have one so we don't have to
  # check every time
  if (!is.function(update_progress))
    update_progress = function(detail) {
      cat(detail, '\n')
    }

  csd = merge_and_split_phenotypes(csd_files, output_dir,
                                   update_progress, col_select)

  # Write out the result
  update_progress(detail='Writing consolidated data.')
  vroom::vroom_write(csd, file.path(output_dir, 'Consolidated_data.txt'),
                     delim='\t', na='#N/A')

  # And the report for the consolidated data
  update_progress(detail='Writing report for consolidated data.')
  write_summary_report(csd=csd,
    output_path=file.path(output_dir, 'Consolidated_data.html'),
    dataset_name='Consolidated data')

  invisible(csd)
}

# Read cell seg data files, split phenotypes into separate columns and
# merge to a single data frame
merge_and_split_phenotypes <- function(csd_files, output_dir,
                                       update_progress, col_select) {
  csd_files = unlist(csd_files)
  if (!is.character(csd_files) || !all(purrr::map_lgl(csd_files, file.exists)))
    stop('Please pass a list of paths to existing cell seg data files.')

  # Make some names, these will be for files and headers
  names = make_unique_names(csd_files)
  field_col = NULL

  # Function to read a file, create a summary report, split phenotypes
  process_one_file <- function(name, path) {
    update_progress(detail=paste0('Reading "', name, '".'))
    d = phenoptr::read_cell_seg_data(path, col_select=col_select)

    # Figure out the field column name from the first file
    if (is.null(field_col))
      field_col <<- phenoptr::field_column(d)

    if (any(!c(field_col, 'Cell ID') %in% names(d)))
      stop('Consolidation requires "', field_col,
           '" and "Cell ID" columns in each data file.')

    dups = duplicated(d[, c(field_col, 'Cell ID')])
    if (sum(dups > 0)) {
      warning('Removing ', sum(dups), ' duplicated rows from ', name,
              '. Did you merge an already merged file?')
      d = d[!dups, ]
    }

    if (!'Slide ID' %in% names(d)) {
      message('Adding Slide ID column to ', basename(path))
      d['Slide ID'] = 'None'
    }

    if (!'Tissue Category' %in% names(d)) {
      message('Adding Tissue Category column to ', basename(path))
      d['Tissue Category'] = 'All'
    }

    # Split before reporting to handle multi-schema phenotyping
    d = d %>% split_phenotypes()

    update_progress(detail=paste0('Writing report for "', name, '".'))
    out_path = file.path(output_dir, paste0(name, '.html'))
    write_summary_report(csd=d, output_path=out_path, dataset_name=name)

    d
  }

  # Process the first file, we will use it as the basis for the result
  csd = process_one_file(names[1], csd_files[1])
  start_row_count = nrow(csd)

  # Read subsequent files, report, split phenotypes, join with the first file.
  purrr::walk2(names[-1], csd_files[-1], function(name, path) {
    # We only need the phenotype columns and join columns from subsequent files
    # Drop everything else for speed and less memory use
    col_select= rlang::quo(list(
      !!rlang::sym(field_col),
      `Cell ID`,
      # These two are required columns, they are added by `process_one_file`
      # if not already present
      dplyr::any_of(c('Tissue Category', 'Slide ID')),
      dplyr::starts_with('Phenotype')))

    csd2 = process_one_file(name, path)

    if (nrow(csd2) != start_row_count)
      stop('Number of rows in data frames do not match.\n',
           nrow(csd2), ' != ', start_row_count, ' Failed at\n', path)
    csd <<- dplyr::inner_join(csd, csd2,
              by=c(field_col, 'Slide ID', 'Tissue Category', 'Cell ID'))

    if (nrow(csd) != start_row_count)
      stop(field_col, 's or Cell IDs do not match (rows dropped in join).\n',
           nrow(csd), ' != ', start_row_count, ' Failed at\n', path)
  })

  csd
}

#' Split all phenotype columns
#'
#' All columns containing multiple phenotypes are split
#' into multiple columns, one for each single phenotype.
#'
#' Multiple phenotypes in the original column must be separated with "/".
#' The names of positive phenotypes must end in "+".
#'
#' @param csd Cell seg data to use.
#' @return A new data frame with `Phenotype` and `Phenotype-<scheme>` columns
#'  replaced with
#'   individual columns per phenotype and the `Confidence` column(s) removed.
#' @importFrom magrittr %>%
#' @export
split_phenotypes = function(csd) {
  # Has this file already been split? If so just return it as-is
  # Split files have phenotype columns like "Phenotype CD8"
  if (sum(stringr::str_detect(names(csd), 'Phenotype ')) > 0) {
    message('Phenotypes are already split, no additional splitting needed.')
    return(csd)
  }

  # Look for classic 'Phenotype' column or multi-schema columns
  # ('Phenotype-<schema name>')
  columns_to_split = stringr::str_subset(names(csd), '^Phenotype(-|$)')
  if (length(columns_to_split) == 0)
    stop('No phenotype columns found.')

  for (column in columns_to_split)
    csd = split_phenotype_column(csd, column)

  csd
}

#' Split a single phenotype column
#'
#' A column containing multiple phenotypes is split
#' into multiple columns, one for each single phenotype.
#'
#' Multiple phenotypes in the original column must be separated with "/".
#' The names of positive phenotypes must end in "+".
#'
#' @param csd Cell seg data to use.
#' @param column The name of the column to split
#' @return A new data frame with the `column` column replaced with
#'   individual columns per phenotype and the `Confidence` column(s) removed.
#' @importFrom magrittr %>%
#' @keywords internal
split_phenotype_column = function(csd, column) {
  # Look for positive phenotypes
  phenotypes = unique(csd[[column]]) %>%
    stringr::str_split('/|(\\s+)') %>% # Split on single / or any whitespace
    purrr::flatten_chr()
  positives = phenotypes[endsWith(phenotypes, '+')] %>% unique()

  if (length(positives) == 0)
    stop('No positive phenotypes found.')

  # If there is no phenotype in the original, leave the new ones blank as well
  blanks = csd[[column]] == ''

  # Make a new column for each positive phenotype
  new_columns = purrr::map(positives, ~{
    # Positive and negative values for the new column
    positive = .x
    negative = stringr::str_replace(positive, '\\+$', '-')

    # Start out all negative or blank
    result = rep(negative, nrow(csd))
    result[blanks] = ''

    # Fill in the positive value anywhere it appears in the original
    original_pos = stringr::str_detect(csd[[column]], stringr::fixed(positive))
    result[original_pos] = positive
    result
  })

  # Make names for the new columns
  new_names = paste('Phenotype', stringr::str_remove(positives, '\\+$'))
  new_columns = new_columns %>% rlang::set_names(new_names)

  # Build a new data frame
  csd %>%
    dplyr::select(-!!rlang::sym(column), -dplyr::contains('Confidence')) %>%
    dplyr::bind_cols(new_columns)
}

# Make unique names from a list of paths to cell seg data files
#
# If the files have unique names, returns the base names without extension.
# If the files do not have unique names, returns the directory name
# plus the file name without extension.
# If those are not unique, appends a sequence number to each name.
# @param csd_files A list or vector of paths to cell seg data files.
# @return A vector of names.
make_unique_names = function(csd_files) {
  # Get the base names sans extension
  names = csd_files %>%
    purrr::map_chr(basename) %>%
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
