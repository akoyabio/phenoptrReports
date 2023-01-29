batch_consolidate <- function(folders_to_consolidate,
                              scan_dir_name=getOption('scan_dir_name'),
                              consolidated_dir_name=getOption('consolidated_dir_name'),
                              require_include=FALSE,
                              col_select=NULL) {
  # Assumptions:
  # 1. cohort folder contains two folders: scan_dir_name (containing qptiff files)
  #    and inForm outputs folder
  # 2. inForm outputs folder for each sample has a merge cell seg data
  # 3. consolidated data doesn't already exist for the samples

  # Check assumption 1
  # Cohort directory is two levels above the folder to consolidate
  cohort_dir_path <- folders_to_consolidate[[1]] %>%
    dirname() %>%
    dirname()

  scan_dir_path <- file.path(cohort_dir_path, scan_dir_name)

  if (!dir.exists(scan_dir_path)) {
    cli::cli_abort("The directory '{scan_dir_name}' is expected in '{cohort_dir_path}' but not found.")
  }

  # Check assumption 2
  csd_files <- lapply(folders_to_consolidate, file.path, "Merge_cell_seg_data.txt") %>% unlist()
  files_not_exist <- csd_files[!file.exists(csd_files)]
  # Generate error message if files are not found
  if (length(files_not_exist) > 0) {
    if (length(files_not_exist) > 1) {
      units <- list("files", "exist", "are")
    } else {
      units <- list("file", "exists", "is")
    }
    cli::cli_h1("Expected Merge {stringr::str_to_title(units[[1]])}")
    cli::cli_ol(files_not_exist)
    cli::cli_abort('The above {length(files_not_exist)} "Merge" {units[[1]]} {units[[3]]} expected but not found.
               Please generate the "Merge" {units[[1]]} before batch processing.')
  }

  # Check assumption 3
  files_to_catch <- expand.grid(folders_to_consolidate, paste0(consolidated_dir_name, "/", 'Consolidated_data.txt')) %>%
    apply(1, paste, collapse="/") %>%
    sort()
  files_exist <- files_to_catch[file.exists(files_to_catch)]
  catch_files_exist_error(files_exist, "Consolidate")

  # Get inputs for the consolidate function
  files_to_process <- lapply(csd_files,
                             get_consolidate_inputs,
                             scan_dir_path=scan_dir_path,
                             consolidated_dir_name=consolidated_dir_name)

  # Run consolidate function for each csd file
  files_to_process %>%
    purrr::iwalk(~consolidate_one_sample(files_to_process_i=.x,
                                        idx=.y,
                                        files_count=length(files_to_process),
                                        require_include=require_include,
                                        col_select=col_select))
  # Write session info
  devtools::session_info(to_file = paste0(dirname(folders_to_consolidate[[1]]), "/R_session_info_", format(Sys.time(), "%Y%m%d"), ".txt"))
}


consolidate_one_sample <- function(files_to_process_i, idx, files_count,
                                   require_include=require_include,
                                   col_select=col_select) {
  # Create output directory if it doesn't exist
  dir.create(files_to_process_i$output_dir, showWarnings = FALSE)

  # Print consolidate sample header
  cli::cli_h1("Consolidating {basename(dirname(files_to_process_i$csd_file))} ({idx} of {files_count})")

  phenoptrReports::consolidate_and_summarize_cell_seg_data(
    csd_files=files_to_process_i$csd_file,
    output_dir=files_to_process_i$output_dir,
    study_dir=files_to_process_i$study_dir,
    export_dir=files_to_process_i$export_dir,
    require_include=require_include,
    col_select=col_select
    )

  # Print status update
  cli::cli_alert_success("Consolidated {basename(dirname(files_to_process_i$csd_file))} ({idx} of {files_count})")
}


get_consolidate_inputs <- function(csd_file, scan_dir_path, consolidated_dir_name) {

  # Get sample name without scan number
  position <- gregexpr("_Scan", basename(dirname(csd_file))) %>% unlist()
  sample_name <- substr(basename(dirname(csd_file)), 1, position-1)

  return(
    list(
      csd_file = csd_file,

      # Write consolidate outputs to consolidated_dir_name
      output_dir = file.path(dirname(csd_file), consolidated_dir_name),

      # Study directory is the whole slide scans directory containing the qptiff files and annotations
      study_dir = file.path(scan_dir_path, sample_name),

      # Export directory is the inForm outputs directory containing 'binary_seg_maps' files
      export_dir = dirname(csd_file)
      )
  )
}
