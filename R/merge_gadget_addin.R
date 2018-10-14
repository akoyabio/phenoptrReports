# This is an RStudio Addin that provides a GUI frontend
# to merge_and_summarize_cell_seg_data.

#' Merge and summarize cell seg data files
#'
#' `merge_addin` opens a GUI that allows you to select multiple
#' cell seg data files. The selected files will be consolidated
#' to a single file which is saved to an output directory
#' along with summary reports for each file.
#' @export
merge_addin = function() {
  intro <- shiny::tagList(shiny::p(
    'This app consolidates multiple inForm cell seg data files',
    'into a single merged file. The merged file will be written',
    'to disk along with summary reports for each data file.'
  ),
      shiny::p(shiny::a('Online Help',
        href='https://akoyabio.github.io/phenoptrReports/articles/consolidation.html')))

  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      .well {
        padding-top: 10px;
        padding-bottom: 5px;
      }
      h3 { margin-top: 10px; }
    "))),
    miniUI::gadgetTitleBar("Consolidate inForm data",
      right=miniUI::miniTitleBarButton('done', 'Process Files', primary=TRUE)),

    miniUI::miniContentPanel(
      intro,

      shiny::wellPanel(
        shiny::h3('Select merge data files'),
        'Click the "Browse Input" button to select one or more',
        'inForm cell seg data files to consolidate.',
        'Continue until all files are selected.',
        shiny::br(), shiny::br(),

        shiny::actionButton('browse_source', 'Browse Input...'),
        shiny::h4('Selected files:'),
        shiny::uiOutput('selected_files')
      ),

      shiny::wellPanel(
      shiny::h3('Select output directory'),
      'Click the "Browse Output" button to select the directory',
      'where the merged data and summary reports will be placed.',
      shiny::br(), shiny::br(),

      shiny::actionButton('browse_output', 'Browse output...'),
      shiny::br(),
      shiny::textOutput('output_dir')
      ),

      shiny::h4(shiny::textOutput('error'), style='color: maroon')
    )
  )

  server <- function(input, output, session) {
    file_list = shiny::reactiveVal()
    output_dir = shiny::reactiveVal()

    # Handle the browse_source button by putting up a file browser
    # to select data files and adding the result to file_list
    shiny::observeEvent(input$browse_source, {
      shiny::req(input$browse_source)
      files = utils::choose.files(
        caption='Select merge data files',
        filters = utils::Filters['txt',])

      if (length(files) == 0)
        return()

      # Only allow cell_seg_data.txt files, and not rejected!
      is_cell_seg = stringr::str_detect(files,
                                        '(?<!rejected)_cell_seg_data.txt$')
      if (!all(is_cell_seg)) {
        shiny::showNotification('Please select only cell_seg_data files!',
                                type='message')
      } else {
        # Add to file_list and update the list of files in the UI
        file_list(unique(c(file_list(), files)))
        output$selected_files = shiny::renderUI({
          do.call(shiny::tagList,
                  purrr::map(file_list(),
                      ~p(paste0(basename(dirname(.x)), '/', basename(.x)))))
        })
      }

      set_error_text()
    })

    # Handle the browse_output button by selecting a folder
    shiny::observeEvent(input$browse_output, {
      shiny::req(input$browse_output)
      output_dir(utils::choose.dir(
        caption='Select an output folder'
      ))

      output$output_dir = shiny::renderText(output_dir())
      set_error_text()
    })

    # Handle the done button by processing files or showing an error
    shiny::observeEvent(input$done, {
      error_text = get_error_text()

      if (error_text == '') {
        progress <- shiny::Progress$new(max=length(file_list())+4)
        progress$set(message = 'Processing files, please wait!',
                     value = 0)
        update_progress <- function(detail = NULL) {
          progress$set(value = progress$getValue()+1, detail = detail)
        }
        phenoptrReports::merge_and_summarize_cell_seg_data(file_list(),
                                                           output_dir(),
                                                           update_progress)
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
      if (is.null(file_list())) {
        'Please select files to process.'
      } else if (is.null(output_dir())) {
        'Please select an output directory.'
      } else
        ''
    }

    # Initialize
    output$error = shiny::renderText('Please select files to process.')
  }

  # Run the gadget in a dialog
  viewer <- shiny::dialogViewer('Merge cell seg data files')
  shiny::runGadget(ui, server, viewer = viewer)
}
