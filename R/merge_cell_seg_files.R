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
merge_cell_seg_files = function(base_path, update_progress=NULL,
                                recursive=FALSE) {

  if (is.null(update_progress))
    update_progress = function(x) cat(x, '\n')

  for (suffix in merge_suffixes) {
    # Get paths to the files matching suffix
    files = list.files(base_path, pattern=suffix,
                       full.names=TRUE, recursive=recursive)
    if (length(files) == 0) next

    update_progress(paste('Merging', length(files), suffix, 'files.'))

    out_path = file.path(base_path, paste0('Merge', suffix))
    merged_data = purrr::map_dfr(files, function(file) {
      update_progress(paste('Reading', file))
      # Read all columns as character to avoid problems with commas (#31)
      readr::read_tsv(file, na='#N/A',
                      col_types = readr::cols(.default=readr::col_character()))
    })

    update_progress(paste('Writing', out_path))
    readr::write_tsv(merged_data, out_path, na='#N/A')
  }
}

# Helper for merge addin computes the number of progress messages we will emit.
merge_progress_count = function(base_path, recursive) {
  # Count the number of files of each type
  counts = merge_suffixes %>%
    purrr::map_int(~length(list.files(base_path, pattern=.x,
                                      recursive=recursive))) %>%
    purrr::keep(~.x>0)

  # Progress advances once for each file and once for each file type
  sum(counts) + length(counts)
}

# Suffixes for candidate files to merge
merge_suffixes = c('_cell_seg_data.txt',
             '_cell_seg_data_summary.txt',
             '_tissue_seg_data.txt',
             '_tissue_seg_data_summary.txt',
             '_score_data.txt',
             '_score_data_summary.txt')


#' Batched merging of inForm output from multiple fields.
#'
#' `batched_merge` merges inForm output from individual fields,
#' creating "Merge" data files in the each subdirectory.
#' @param base_path Path to a directory containing files to merge. All
#' eligible files in this directory will be merged.
#' @export
batched_merge <- function(base_path) {

  base_path <- gsub("\\\\", replacement = "/", base_path)

  message <- glue::glue('
  batched_merge(base_path = "{base_path}")\n
                        ')
  cat(message)

  # perform check on directory
  if (!dir.exists(base_path)) {
    message <- glue::glue('\n{base_path} does not exist.\n')
    stop(message)
  }

  subdir <- list.dirs(base_path, recursive = FALSE)

  if (length(subdir) == 0) {
    message <- glue::glue('\n{base_path} does not contain a subdirectory.\n')
    stop(message)
  }

  # keep track of which subdirectories were processed with merge function
  subdir_written <- list()

  # process each subdirectory
  for (i in seq_along(subdir)) {
    merge_cell_seg_files(subdir[[i]], update_progress=NULL,
                         recursive=FALSE)

    if (merge_file_exists(subdir[[i]])) {
      subdir_written[[length(subdir_written)+1]] <- subdir[[i]]
    }
  cat("\n")
  }

  subdir_written <- subdir_written %>% unlist() %>% as.vector()
  subdir_not_written <- setdiff(subdir %>% as.vector(), subdir_written)

  # write text file and print status message to console
  write_batch_merge_status_txt(base_path, subdir_written, subdir_not_written)
}

merge_file_exists <- function(subdir_i) {
  merged_file_names = c('Merge_cell_seg_data.txt',
                        'Merge_cell_seg_data_summary.txt',
                        'Merge_tissue_seg_data.txt',
                        'Merge_tissue_seg_data_summary.txt',
                        'Merge_score_data.txt',
                        'Merge_score_data_summary.txt')

  merge_cell_seg_file_path <- file.path(subdir_i, merged_file_names)
  any(file.exists(merge_cell_seg_file_path))
}


write_batch_merge_status_txt <- function(base_path, subdir_written, subdir_not_written) {
  date <- Sys.time()
  line1 <- glue::glue('Batched merge cell seg data processed on {date}.\n\n')

  subdir_length <- length(subdir_written) + length(subdir_not_written)
  subdir_units <- get_units(subdir_length)

  line2 <- glue::glue('{base_path} containing {subdir_length} {subdir_units[[1]]} {subdir_units[[2]]} submitted for processing.\n\n')

  subdir_written_length <- length(subdir_written)
  subdir_written_units <- get_units(subdir_written_length)

  line3 <- glue::glue('{subdir_written_length} of {subdir_length} {subdir_units[[1]]} {subdir_written_units[[2]]} processed:\n')

  if (subdir_written_length > 0) {
    line4 <- paste0(subdir_written)
  } else {
    line4 <- "No subdirectory was processed."
  }

  subdir_not_written_length <- length(subdir_not_written)
  subdir_not_written_units <- get_units(subdir_not_written_length)

  line5 <- glue::glue('\n\n{subdir_not_written_length} of {subdir_length} {subdir_units[[1]]} {subdir_not_written_units[[2]]} not processed:\n')

  if (subdir_not_written_length > 0) {
    line6 <- paste0(subdir_not_written)
  } else {
    line6 <- "All subdirectories were processed."
  }

  # write status to text file
  writeLines(c(line1,
               line2,
               line3,
               line4,
               line5,
               line6),
             con = file.path(base_path, "Batched_Merge_Report.txt"),
             sep = "\n")

  # print status to console
  cat(c("\nDone!\n",
        line1,
        line2,
        line3,
        line4,
        line5,
        line6),
      sep = "\n")
}

get_units <- function(num) {
  if (num > 1) {
    units <- c("subdirectories", "were")
  } else {
    units <- c("subdirectory", "was")
  }
}
