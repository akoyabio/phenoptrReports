# This is an RStudio Addin that provides a GUI frontend
# to signal_to_noise_report.

#' Generate a signal-to-noise (crosstalk) report for a selected export folder.
#'
#' `signal_to_noise_addin` opens a GUI that allows you to select
#' an export folder containing component data files for singleplex samples.
#' A signal-to-noise report is created for the samples and saved in
#' the source directory.
#'
#' The report generator tries to identify the Opal fluor for each source file
#' by looking at the file name. It recognizes "DAPI", "AF", and "Opal_nnn".
#' It also recognizes three leading digits as the number of an Opal fluor.
#' @export
signal_to_noise_addin = function() {
  intro <- shiny::tagList(shiny::p(
    'This app reads component data files for singleplex samples',
    'and creates a signal-to-noise report showing crosstalk between the samples.'
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
    miniUI::gadgetTitleBar("Signal-to-noise report",
      right=miniUI::miniTitleBarButton('done', 'Create Report', primary=TRUE)),

    miniUI::miniContentPanel(
      intro,

      shiny::wellPanel(
      shiny::h3('Select Export directory'),
      'Click the "Browse" button to select a directory',
      'containing component data files for singly-stained samples.',
      shiny::br(), shiny::br(),

      shiny::actionButton('browse', 'Browse...'),
      shiny::br(),
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
      export_dir(utils::choose.dir(
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
        phenoptrReports::signal_to_noise_report(export_dir())
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
  viewer <- shiny::dialogViewer('Create signal-to-noise report')
  shiny::runGadget(ui, server, viewer = viewer)
}
