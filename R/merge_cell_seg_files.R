#' Merge inForm output from multiple fields.
#'
#' `merge_cell_seg_files` merges inForm output from individual fields,
#' creating "Merge" data files. This is similar to the inForm Merge tab but
#' does not include the ability to review and reject individual fields.
#' @param base_path Path to a directory containing files to merge. All
#' eligible files in this directory will be merged.
#' @param update_progress Callback function which is called with progress.
#' @param recursive If TRUE, will find files in subdirectories of `base_path`.
#' @export
merge_cell_seg_files = function(base_path,
                                update_progress = NULL,
                                recursive = FALSE) {
  # Check if merged files exist in base_path. If so, display error.
  files_to_catch <-
    file.path(base_path, paste0('Merge', merge_suffixes))

  files_exist <- files_to_catch[file.exists(files_to_catch)]
  catch_files_exist_error(files_exist, "Merge")

  if (is.null(update_progress))
    update_progress = function(x)
      cat(x, '\n')

  for (suffix in merge_suffixes) {
    # Get paths to the files matching suffix
    files = list.files(
      base_path,
      pattern = suffix,
      full.names = TRUE,
      recursive = recursive
    )
    if (length(files) == 0)
      next

    update_progress(paste('Merging', length(files), suffix, 'files.'))

    out_path = file.path(base_path, paste0('Merge', suffix))
    merged_data = purrr::map_dfr(files, function(file) {
      update_progress(paste('Reading', file))
      # Read all columns as character to avoid problems with commas (#31)
      readr::read_tsv(
        file,
        na = '#N/A',
        col_types = readr::cols(.default = readr::col_character())
      )
    })

    update_progress(paste('Writing', out_path))
    readr::write_tsv(merged_data, out_path, na = '#N/A')
  }
}

# Helper for merge addin computes the number of progress messages we will emit.
merge_progress_count = function(base_path, recursive) {
  # Count the number of files of each type
  counts = merge_suffixes %>%
    purrr::map_int( ~ length(list.files(
      base_path, pattern = .x,
      recursive = recursive
    ))) %>%
    purrr::keep( ~ .x > 0)

  # Progress advances once for each file and once for each file type
  sum(counts) + length(counts)
}

# Suffixes for candidate files to merge
merge_suffixes = c(
  '_cell_seg_data.txt',
  '_cell_seg_data_summary.txt',
  '_tissue_seg_data.txt',
  '_tissue_seg_data_summary.txt',
  '_score_data.txt',
  '_score_data_summary.txt'
)

catch_files_exist_error <- function(files_exist, file_type) {
  if (length(files_exist) > 0) {
    units <- get_units(length(files_exist))

    error_message <- function(files_exist) {
      cli::cli_h1("Existing {file_type} {stringr::str_to_title(units[[1]])}")
      cli::cli_ol(files_exist)

      if (file_type == "Merge") {
        cli::cli_abort(
          'The above {length(files_exist)} "{file_type}" {units[[1]]} already {units[[2]]}.
               Please remove the {units[[1]]} before batch processing.'
        )
      } else if (file_type == "Consolidate") {
        cli::cli_abort(
          'The above {length(files_exist)} "{file_type}" {units[[1]]} already {units[[2]]}.
               Please remove all associated "{file_type}" files before batch processing.'
        )
      }
    }
    error_message(files_exist)
  }
}

get_units <- function(num) {
  if (num > 1) {
    units <- c("files", "exist")
  } else {
    units <- c("file", "exists")
  }
}
