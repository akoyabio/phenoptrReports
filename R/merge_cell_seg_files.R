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

  for (suffix in merge_suffixes)
  {
    # Get paths to the files matching suffix
    files = list.files(base_path, pattern=suffix,
                       full.names=TRUE, recursive=recursive)
    if (length(files) == 0) next

    update_progress(paste('Merging', length(files), suffix, 'files.'))

    append = FALSE
    outPath = file.path(base_path, paste0('Merge', suffix))
    for (file in files)
    {
      update_progress(file)
      data = readr::read_tsv(file, na='#N/A')
      readr::write_tsv(data, outPath, na='#N/A', append=append)
      append = TRUE
    }
  }
}

# Helper for merge addin computes the number of progress messages we will emit.
merge_progress_count = function(base_path, recursive) {
  # Count the number of files of each type
  counts = merge_suffixes %>%
    purrr::map_int(~length(list.files(base_path, pattern=.x, recursive=recursive))) %>%
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
