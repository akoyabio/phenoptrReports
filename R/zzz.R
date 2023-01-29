.onLoad <- function(libname, pkgname) {
  # Set name of the merge and consolidate folde
  op <- options()
  my_opts = list(
    consolidated_dir_name="Consolidated Data",
    scan_dir_name="Whole Slide Scans")
  toset = !(names(my_opts) %in% names(op))
  if(any(toset)) options(my_opts[toset])
}
