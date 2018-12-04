# This is an RStudio Addin that provides a GUI frontend
# to merge_cell_seg_data.

#' Merge cell seg data files
#'
#' `addin_10_merge` opens a GUI that allows you to select a directory containing
#' cell seg data files from multiple, individual fields. The selected files
#' will be merged
#' to a single file which is saved to the source directory.
#'  This is similar to the function of the inForm Merge tab but
#' does not include the ability to review and reject individual fields.
#' @export
addin_10_merge = function() {
  intro <- shiny::tagList(shiny::p(
      'This app merges multiple inForm cell seg data files',
      'into a single file. The merged file will be written',
      'to the source directory.'
    ),
    shiny::p('The source directory should contain inForm data files',
             'created from individual fields.'),
    shiny::p(shiny::a('Online Help',
      href='https://akoyabio.github.io/phenoptrReports/articles/merging.html')))

  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      .well {
        padding-top: 10px;
        padding-bottom: 5px;
      }
      h3 { margin-top: 10px; }
    "))),
    miniUI::gadgetTitleBar("Merge inForm data",
      right=miniUI::miniTitleBarButton('done', 'Process Files', primary=TRUE)),

    miniUI::miniContentPanel(
      intro,

      shiny::wellPanel(
        shiny::h3('Select source directory'),
        'Click the "Browse Input" button to select a directory containing',
        'inForm cell seg data files to merge.',

        shiny::br(), shiny::br(),

        shiny::actionButton('browse_source', 'Browse Input...'),
        shiny::h4('Selected directory:'),
        shiny::textOutput('source_dir')
      ),

      shiny::h4(shiny::textOutput('error'), style='color: maroon')
    )
  )

  server <- function(input, output, session) {
    source_dir = shiny::reactiveVal()

    # Handle the browse_source button by selecting a folder
    shiny::observeEvent(input$browse_source, {
      shiny::req(input$browse_source)
      source_dir(utils::choose.dir(
        caption='Select a source folder'
      ))

      output$source_dir = shiny::renderText(source_dir())
      set_error_text()
    })

    # Handle the done button by processing files or showing an error
    shiny::observeEvent(input$done, {
      error_text = get_error_text()

      if (error_text == '') {
        progress <- shiny::Progress$new(
          max=merge_progress_count(source_dir()))
        progress$set(message = 'Processing files, please wait!',
                     value = 0)
        update_progress <- function(detail = NULL) {
          progress$set(value = progress$getValue()+1, detail = detail)
        }

        phenoptrReports::merge_cell_seg_files(source_dir(), update_progress)
        update_progress(detail='Done!')
        Sys.sleep(0.5)
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
      if (is.null(source_dir())) {
        'Please select a source directory to process.'
      } else if (length(list.files(source_dir(),
                                   pattern='Merge', ignore.case=TRUE)) > 0) {
        'Please select a source directory which does not contain existing merge files.'
      } else if (merge_progress_count(source_dir())==0) {
        'Please a source directory containing inForm output files.'
      } else
        ''
    }

    # Initialize
    output$error = shiny::renderText('Please a source directory to process.')
  }

  # Run the gadget in a dialog
  viewer <- shiny::dialogViewer('Merge cell seg data files')
  shiny::runGadget(ui, server, viewer = viewer)
}
