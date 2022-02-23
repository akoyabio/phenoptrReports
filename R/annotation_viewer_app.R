#' Run the Annotation Viewer app
#'
#' Runs a Shiny app which will read a GeoJSON annotation file and
#' display it
#' @export
addin_80_annotation_viewer_app = function() {
  opts = options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
  on.exit(options(opts))
  shiny::runApp(system.file('annotation_viewer_app',
                            package='phenoptrReports'))
}
