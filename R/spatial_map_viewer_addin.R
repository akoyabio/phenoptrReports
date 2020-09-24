# This is an RStudio Addin that provides a field-based viewer of
# nearest neighbor relationships

# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(
  c('.csd_path', '.export_path'))

#' Spatial map viewer addin
#'
#' This function starts a GUI which allows you to select a
#' `Consolidated_data.txt` file and an inForm image directory.
#' Once selected, you can view fields from the image directory with a
#' superimposed map of nearest neighbor relationships.
#' @export
addin_35_spatial_map_viewer = function() {
  # Run the front end to get the nn file and export directory
  spatial_map_viewer_front_end()
}


#' Run the spatial map viewer with the given data
#' file and export directory.
#'
#' See [addin_35_spatial_map_viewer()]
#' for a GUI front-end to this function.
#' @param csd_path Path to a Consolidated_data.txt file from the consolidation
#' app or nearest_neighbors.txt or count_within.txt
#' file created by the analysis app.
#' @param export_path Path to a directory containing composite and
#' component images for the fields in the data file.
#' @return None; starts the viewer app
#' @export
spatial_map_viewer = function(csd_path, export_path) {
  # Apparently the best way to pass parameters to a shiny app is
  # to modify the global environment :-(
  # Use . names to avoid collisions with existing variables
  .GlobalEnv$.csd_path=csd_path
  .GlobalEnv$.export_path = export_path
  on.exit(rm(.csd_path, .export_path, envir=.GlobalEnv))

  cat('Starting app\n')
  shiny::runApp(system.file('spatial_map_viewer_app',
                            package='phenoptrReports'),
                launch.browser=TRUE)
}


# Request input file and directory for the viewer, then launch the viewer
spatial_map_viewer_front_end = function() {
  intro <- shiny::tagList(shiny::p(
      'This app provides a field-based viewer',
      'of nearest neighbor relationships. ',
      'It requires a Consolidated_data.txt file from the consolidation ',
      ' app or a nearest_neighbors.txt or count_within.txt ',
      'file created by the analysis app, ',
      'and an inForm export directory containing composite and component ',
      'image files.'
    ))

  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      .well {
        padding-top: 10px;
        padding-bottom: 5px;
      }
      h3 { margin-top: 10px; }
    "))),
    miniUI::gadgetTitleBar("Spatial map viewer",
      right=miniUI::miniTitleBarButton('done', 'Open Viewer', primary=TRUE)),

    miniUI::miniContentPanel(
      intro,

      shiny::wellPanel(
        shiny::h3('Select a data file'),
        'Click the "Browse Input" button to select a ',
        'Consolidated_data.txt, ',
        'nearest_neighbors.txt or count_within.txt file.',
        shiny::br(), shiny::br(),

        shiny::actionButton('browse_source', 'Browse Input...'),
        shiny::br(),
        shiny::uiOutput('selected_file')
      ),

      shiny::wellPanel(
      shiny::h3('Select an inForm image directory'),
      'Click the "Browse" button to select an inForm export directory ',
      'containing composite and component image files for the fields in ',
      'the data file.',
      shiny::br(), shiny::br(),

      shiny::actionButton('browse_export', 'Browse...'),
      shiny::br(),
      shiny::textOutput('export_dir')
      ),

      shiny::h4(shiny::textOutput('error'), style='color: maroon')
    )
  )

  server <- function(input, output, session) {
    data_file = shiny::reactiveVal()
    export_dir = shiny::reactiveVal()
    default_dir = ''

    # Handle the browse_source button by putting up a file browser
    # to a data file.
    shiny::observeEvent(input$browse_source, {
      shiny::req(input$browse_source)

      default = dplyr::if_else(default_dir=='', '',
                               paste0(default_dir, '/*.txt'))
      files = phenoptrReports::choose_files(
        default=default, multi=FALSE,
        caption='Select a data file',
        filters = c("CSV files (*.csv)", "*.csv",
                    "Text files (*.txt)", "*.txt"))

      if (length(files) == 0)
        return()

      # Set the default directory even if nothing valid is selected
      default_dir <<- dirname(files[1])

      # Only allow _data.txt, count_within.txt or nearest_neighbors.txt files!
      is_nn = stringr::str_detect(files,
                '(_data\\.txt|nearest_neighbors.*\\.txt|count_within.*\\.txt)$')
      if (!all(is_nn)) {
        shiny::showNotification(
          'Please select a Consolidated_data.txt, nearest_neighbors.txt or count_within.txt file!',
          type='message')
      } else {
        # Save to data_file and update the UI
        data_file(files)
        output$selected_file = shiny::renderUI({
          shiny::p(paste0(basename(dirname(files)), '/', basename(files)))
        })

        # Use the selected file's directory if one has not been set
        if (!shiny::isTruthy(export_dir())) export_dir(default_dir)
      }
    })

    # Handle the browse_output button by selecting a folder
    shiny::observeEvent(input$browse_export, {
      shiny::req(input$browse_export)
      selected_dir = phenoptrReports::choose_directory(
        default=default_dir,
        caption='Select an inForm image directory'
      )

      if (shiny::isTruthy(selected_dir)) export_dir(selected_dir)
    })

    output$export_dir = shiny::renderText(export_dir())

    # Handle the done button by opening the viewer or showing an error
    shiny::observeEvent(input$done, {
      if (error_text() == '') {
        # This is kind of a hack to open a Shiny app from a Shiny gadget
        # (like this one).
        # https://stackoverflow.com/questions/44891544/how-to-open-a-shiny-app-from-within-an-rstudio-gadget
        data_file = stringr::str_replace_all(data_file(), '\\\\', '/')
        export_dir = stringr::str_replace_all(export_dir(), '\\\\', '/')
        command <- stringr::str_glue(
          "phenoptrReports::spatial_map_viewer('{data_file}', '{export_dir}')")
        rstudioapi::sendToConsole(command)
        shiny::stopApp()
      } else {
        shiny::showNotification(error_text(), type='message')
      }
    })

    # Handle the cancel button by quitting
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

    output$error = shiny::renderText({ error_text() })

    error_text = shiny::reactive({
      if (!shiny::isTruthy(data_file())) {
        'Please select a data file to process.'
      } else if (!shiny::isTruthy(export_dir())) {
        'Please select an inForm image directory.'
      } else
        ''
    })
  }

  # Run the gadget in a dialog
  viewer <- shiny::dialogViewer('Spatial map viewer')
  shiny::runGadget(ui, server, viewer = viewer)
}
