addin_15_batch_merge <- function() {
  intro <- shiny::tagList(
    shiny::p('Some text, ',
             'some more text, ',
             'and some final text.'),
    shiny::p(
      'The source directory should contain inForm data files. ',
      'Data for each slide should be organized in subdirectories.'
    )
  )

  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shiny::tags$head(shiny::tags$style(
      shiny::HTML(
        "
      .well {
        padding-top: 10px;
        padding-bottom: 5px;
      }
      h3 { margin-top: 10px; }
    "
      )
    )),

    miniUI::gadgetTitleBar(
      "Batch Processing of Merge inForm Data Files",
      right = shinyjs::disabled(
        miniUI::miniTitleBarButton('done',
                                   'Process Files',
                                   primary = TRUE)
      )
    ),

    miniUI::miniContentPanel(
      intro,

      shiny::wellPanel(
        shiny::h3('Select source directory'),
        'Click the "Browse Input" button to select a directory containing',
        'inForm cell seg data files to merge.',

        shiny::br(),
        shiny::br(),

        shiny::actionButton('browse_source', 'Browse Input...'),
        shiny::h4('Selected directory:'),
        shiny::textOutput('source_dir')
      ),

      shiny::uiOutput("sample_selection_panel"),
      shiny::br(),
      shiny::br(),
      shiny::h4(shiny::textOutput('error'), style = 'color: maroon')
    )
  )

  server <- function(input, output, session) {
    source_dir <- shiny::reactiveVal()

    # Handle the browse_source button by selecting a folder
    shiny::observeEvent(input$browse_source, {
      shiny::req(input$browse_source)
      source_dir(phenoptrReports::choose_directory(caption = 'Select a source folder'))

      output$source_dir = shiny::renderText(source_dir())

      # Make UI for sample selection
      output$sample_selection_panel <- shiny::renderUI(
        shiny::wellPanel(
          shiny::h3('Select sample(s) for processing'),
          'Select the sample(s) to perform merging of inForm data.',

          shiny::br(),
          shiny::br(),
          shiny::actionButton("all", "Select All"),
          shiny::actionButton("none", "Select None"),
          shiny::br(),
          shiny::br(),
          shinyWidgets::multiInput(
            inputId = "sample_list",
            label = NULL,
            choices = list.dirs(
              source_dir(),
              full.names = FALSE,
              recursive = FALSE
            ),
            width = "800px",
            options = list(
              enable_search = TRUE,
              non_selected_header = "Available Samples:",
              selected_header = "Samples Selected for Processing:"
            )
          )
        )
      )

      set_error_text()
    })

    # Handle select all button
    shiny::observeEvent(input$all, {
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "sample_list",
        selected = list.dirs(
          source_dir(),
          full.names = FALSE,
          recursive = FALSE
        )
      )
    })

    # Handle select none button
    shiny::observeEvent(input$none, {
      shinyWidgets::updateMultiInput(session = session,
                                     inputId = "sample_list",
                                     selected = character(0))
    })

    # Handle the enabling of done button
    shiny::observe({
      if (length(input$sample_list) > 0) {
        shinyjs::enable(id = 'done')
      } else {
        shinyjs::disable(id = 'done')
      }
    })

    # Handle the done button by processing files or showing an error
    shiny::observeEvent(input$done, {
      # Prevent resubmission
      shinyjs::disable(id = 'done')

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Batch Processing in Progress",
        text = "Please keep app open until processing is complete. See console for status updates.",
        type = "info"
      )

      # Construct full paths of sample folders and process data
      folders_to_merge <-
        expand.grid(source_dir(), input$sample_list) %>%
        apply(1, paste, collapse = "/") %>%
        sort()
      batch_merge(folders_to_merge)

      shiny::stopApp()
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
      } else if (length(input$sample_list) == 0) {
        'Please select one or more samples for batch processing.'
      } else
        ''
    }

    # Initialize
    output$error = shiny::renderText('Please select a source directory to process.')
  }

  # Run the gadget in a dialog
  viewer <-
    shiny::dialogViewer('Batch Processing of Merge Cell Seg Data Files',
                        width = 900,
                        height = 1000)
  shiny::runGadget(ui, server, viewer = viewer)
}
