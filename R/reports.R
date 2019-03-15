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

  # Fail soft so calling scripts can continue.
  # consolidate_and_summarize_cell_seg_data, in particular, has less
  # stringent requirements than the summary report.
  tryCatch(rmarkdown::render(rmd_path, output_file=output_path, quiet=TRUE,
                    params=list(csd_path=csd_path, csd=csd,
                                dataset_name=dataset_name)),
           error = function(e) {
             cat('Unable to write summary report\n', e$message, '\n')
           })
}

#' Create summary charts from the results of an analysis
#'
#' Creates a Microsoft Word file or HTML report containing summary charts
#' derived from an analysis. The file type is determined by the file extension
#' of `output_path`, which must be either `.docx` or `.html`.
#' @param workbook_path Path to an Excel file containing sheets written
#'   by [write_counts_sheet], etc.
#' @param output_path Path to write the resulting file.
#' @export
write_summary_charts = function(workbook_path, output_path) {
  stopifnot(file.exists(workbook_path))

  if (is.null(output_path))
    stop('You must provide an output path.')

  output_format = switch(tools::file_ext(output_path),
                   docx='word_document',
                   html='html_vignette')

  if (is.null(output_format))
    stop('Unsupported output format')

  rmd_path = system.file("rmd", "Chart_report.Rmd",
                         package="phenoptrReports")

  rmarkdown::render(rmd_path, output_file=output_path, quiet=TRUE,
                    output_format=output_format,
                    params=list(workbook_path=workbook_path))
}

#' Create an unmixing quality report for simplex samples
#'
#' `export_path` should be the path to
#' an export folder containing component data files for singleplex samples.
#' An unmixing quality report is created for the samples and saved in
#' the source directory.
#'
#' The report generator tries to identify the Opal fluor for each source file
#' by looking at the file name. It recognizes "DAPI", "AF", "Opalnnn"
#' and "Opal_nnn".
#' It also recognizes three leading digits as the number of an Opal fluor.
#' @param export_path Path to a directory containing component_data files.
#' @export
unmixing_quality_report = function(export_path=NULL) {
  stopifnot(dir.exists(export_path))

  rmd_path = system.file("rmd", "Unmixing_quality_report.Rmd",
                         package="phenoptrReports")

  output_path = file.path(export_path, 'Unmixing_quality_report.html')
  rmarkdown::render(rmd_path, output_file=output_path, quiet=TRUE,
                    params=list(export_path = export_path))
}

#' Create a component levels report for multiplex samples
#'
#' `export_path` should be the path to
#' an export folder containing component data files for multiplex samples.
#' A component levels report is created for the samples and saved in
#' the source directory.
#' @param export_path Path to a directory containing component_data files.
#' @export
component_levels_report = function(export_path=NULL) {
  stopifnot(dir.exists(export_path))

  rmd_path = system.file("rmd", "Component_levels_report.Rmd",
                         package="phenoptrReports")

  output_path = file.path(export_path, 'Component_levels_report.html')
  rmarkdown::render(rmd_path, output_file=output_path, quiet=TRUE,
                    params=list(export_path = export_path))
}
