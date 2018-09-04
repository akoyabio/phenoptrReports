#' Create a summary report for a cell seg data file
#'
#' Creates an HTML report summarizing the slides, fields and phenotypes
#' in a single cell seg data file.
#' @param csd_path Path to cell seg data file, or NULL.
#' @param csd Cell seg table, or NULL.
#' @param output_path Path to write the resulting HTML file.
#' @param dataset_name Descriptive name of the dataset.
#' @export
write_summary_report = function(csd_path=NULL, csd=NULL,
                                dataset_name=NULL, output_path) {
  if (is.null(csd_path) == is.null(csd))
    stop('Provide csd_path or csd but not both.')

  if (is.null(output_path))
    stop('You must provide an output path.')

  rmd_path = system.file("rmd", "Cell_seg_summary_report.Rmd",
                         package="phenoptrReports")

  rmarkdown::render(rmd_path, output_file=output_path,
                    params=list(csd_path=csd_path, csd=csd,
                                dataset_name=dataset_name))
}
