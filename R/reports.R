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

#' Create summary charts from the results of an analysis
#'
#' Creates a Microsoft Word file or HTML report containing summary charts
#' derived from an analysis. The file type is determined by the file extension
#' of `output_path`, which must be either `.docx` or `.html`.
#' @param worksheet_path Path to an Excel file containing sheets written
#'   by [write_counts_sheet], etc.
#' @param output_path Path to write the resulting file.
#' @export
write_summary_charts = function(worksheet_path, output_path) {
  stopifnot(file.exists(worksheet_path))

  if (is.null(output_path))
    stop('You must provide an output path.')

  output_format = switch(tools::file_ext(output_path),
                   docx='word_document',
                   html='html_vignette')

  if (is.null(output_format))
    stop('Unsupported output format')

  rmd_path = system.file("rmd", "Chart_report.Rmd",
                         package="phenoptrReports")

  rmarkdown::render(rmd_path, output_file=output_path,
                    output_format=output_format,
                    params=list(data_path=worksheet_path))
}

#' Create a signal-to-noise report for simplex samples
#'
#' @param export_path Path to a directory containing component_data files.
#' @export
signal_to_noise_report = function(export_path=NULL) {
  stopifnot(dir.exists(export_path))

  rmd_path = system.file("rmd", "Signal_to_noise.Rmd",
                         package="phenoptrReports")

  output_path = file.path(export_path, 'Signal_to_noise.html')
  rmarkdown::render(rmd_path, output_file=output_path,
                    params=list(export_path = export_path))
}
