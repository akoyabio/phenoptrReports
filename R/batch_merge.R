batch_merge <- function(folders_to_merge) {
  # Check if merged files already exist. If so, display error.
  files_to_catch <- expand.grid(folders_to_merge, paste0('Merge', merge_suffixes)) %>%
    apply(1, paste, collapse="/") %>%
    sort()
  files_exist <- files_to_catch[file.exists(files_to_catch)]
  catch_files_exist_error(files_exist, "Merge")

  # Merge the samples
  folders_count <- length(folders_to_merge)
  folders_to_merge %>%
    purrr::iwalk(~merge_one_sample(folders_to_merge_i=.x,
                                   idx=.y,
                                   folders_count=folders_count))

  # Write session info
  devtools::session_info(to_file = paste0(dirname(folders_to_merge[[1]]), "/R_session_info_", format(Sys.time(), "%Y%m%d"), ".txt"))
}

merge_one_sample <- function(folders_to_merge_i, idx, folders_count) {
  # Print merge sample header
  cli::cli_h1("Merging {basename(folders_to_merge_i)} ({idx} of {folders_count})")
  phenoptrReports::merge_cell_seg_files(base_path=folders_to_merge_i,
                                        recursive=FALSE)
  # Print status update
  cli::cli_alert_success("Merged {basename(folders_to_merge_i)} ({idx} of {folders_count})")
}
