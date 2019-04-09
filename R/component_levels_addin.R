# This is an RStudio Addin that provides a GUI frontend
# to component_levels_report.

#' Generate a component levels report for a selected export folder.
#'
#' `addin_50_component_levels` opens a GUI that allows you to select
#' an export folder containing component data files for multiplex samples.
#' A component levels report is created for the samples and saved in
#' the export directory.
#' @export
addin_50_component_levels = function() {
  intro <- shiny::tagList(shiny::p(
    'This app reads component data files for multiplex samples',
    'and creates a component levels report showing signal levels',
    'for bright and dark pixels in each sample.'
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
    miniUI::gadgetTitleBar("Component levels report",
      right=miniUI::miniTitleBarButton('done', 'Create Report', primary=TRUE)),

    miniUI::miniContentPanel(
      intro,

      shiny::wellPanel(
      shiny::h3('Select Export directory'),
      'Click the "Browse" button to select a directory',
      'containing component data files for multiplex samples.',
      shiny::br(), shiny::br(),

      shiny::actionButton('browse', 'Browse...'),
      shiny::br(), shiny::br(),
      shiny::textOutput('export_dir')
      ),

      shiny::h4(shiny::textOutput('error'), style='color: maroon')
    )
  )

  server <- function(input, output, session) {
    file_list = shiny::reactiveVal()
    export_dir = shiny::reactiveVal()

    # Handle the browse button by selecting a folder
    shiny::observeEvent(input$browse, {
      shiny::req(input$browse)
      export_dir(phenoptrReports::choose_directory(
        caption='Select an export folder'
      ))

      output$export_dir = shiny::renderText(export_dir())
      set_error_text()
    })

    # Handle the done button by processing files or showing an error
    shiny::observeEvent(input$done, {
      error_text = get_error_text()

      if (error_text == '') {
        shiny::showNotification('Processing, please wait!', duration=NULL,
                                closeButton=FALSE, type='message')
        phenoptrReports::component_levels_report(export_dir())
        shiny::stopApp()
      } else {
        shiny::showNotification(error_text, type='message')
      }
    })

    # Handle the cancel button by quitting
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

    # Set error message in response to user input
    # For some reason this doesn't work as a reactive so just make
    # it a function and call as needed
    set_error_text = function() {
      output$error = shiny::renderText(get_error_text())
    }

    get_error_text = function() {
      if (is.null(export_dir())) {
        'Please select an output directory.'
      } else if (length(list.files(export_dir(), pattern='component_data.tif'))==0) {
        'Please select an export directory containing component data files.'
      } else ''
    }

    # Initialize
    output$error = shiny::renderText('Please select an output directory.')
  }

  # Run the gadget in a dialog
  viewer <- shiny::dialogViewer('Create component levels report')
  shiny::runGadget(ui, server, viewer = viewer)
}
