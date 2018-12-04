#' Run the Analysis app
#'
#' Runs a Shiny app which will read a consolidated data file and
#' create aggregated statistics from it.
#' @export
addin_30_analysis_app = function() {
  shiny::runApp(system.file('analysis_app', package='phenoptrReports'))
}
