#' Run the Analysis app
#'
#' Runs a Shiny app which will read a consolidated data file and
#' create aggregated statistics from it.
#' @export
addin_30_analysis_app = function() {
  opts = options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
  on.exit(options(opts))
  shiny::runApp(system.file('analysis_app', package='phenoptrReports'))
}
