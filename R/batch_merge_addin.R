addin_15_batch_merge = function() {



  fruits <- c("Banana", "Blueberry", "Cherry",
              "Coconut", "Grapefruit", "Kiwi",
              "Lemon", "Lime", "Mango", "Orange",
              "Papaya")

  intro <- shiny::tagList(shiny::p(
    'Some text, ',
    'some more text, ',
    'and some final text.'
  ),
  shiny::p('The source directory should contain inForm data files. ',
           'Data for each slide should be organized in subdirectories.'))

  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      .well {
        padding-top: 10px;
        padding-bottom: 5px;
      }
      h3 { margin-top: 10px; }
    "))),

    miniUI::gadgetTitleBar("Batch Processing of Merge inForm Data Files",
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

      shiny::wellPanel(
        shiny::h3('Select sample(s) for processing'),
        'Select the sample(s) to perform merging of inForm data.',

        shiny::br(), shiny::br(),
        shiny::actionButton("all", "Select All"),
        shiny::actionButton("none", "Select None"),
        shiny::br(), shiny::br(),
        shinyWidgets::multiInput(
          inputId = "id", label = NULL,
          choices = fruits, width = "800px",
          options = list(
            enable_search = TRUE,
            non_selected_header = "Available Samples:",
            selected_header = "Samples Selected for Processing:"
          ),
        ),

        shiny::verbatimTextOutput(outputId = "res")
      ),

      shiny::h4(shiny::textOutput('error'), style='color: maroon')
    )
  )

  server <- function(input, output, session) {
    source_dir <- shiny::reactiveVal()
    sample_list <- shiny::reactive({
      dir(source_dir())
    })

    # Handle the browse_source button by selecting a folder
    shiny::observeEvent(input$browse_source, {
      shiny::req(input$browse_source)
      source_dir(phenoptrReports::choose_directory(
        caption='Select a source folder'
      ))

      sample_list(dir(source_dir()))

      output$source_dir = shiny::renderText(source_dir())
      set_error_text()
    })

    # Update sample selection
    output$res <- renderPrint({
      input$id
    })

    # Handle select all button
    shiny::observeEvent(input$all, {
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "id",
        selected = fruits
      )
    })

    # Handle select none button
    observeEvent(input$none, {
      shinyWidgets::updateMultiInput(
        session = session,
        inputId = "id",
        selected = character(0)
      )
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

        phenoptrReports::merge_cell_seg_files(source_dir(),
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
      if (is.null(source_dir())) {
        'Please select a source directory to process.'
      } else if (length(list.files(source_dir(),
                                   pattern='Merge', ignore.case=TRUE)) > 0) {
        'Please select a source directory which does not contain existing merge files.'
      } else
        ''
    }

    # Initialize
    output$error = shiny::renderText('Please a source directory to process.')
  }

  # Run the gadget in a dialog
  viewer <- shiny::dialogViewer('Batch Processing of Merge Cell Seg Data Files', width=900, height=1000)
  shiny::runGadget(ui, server, viewer = viewer)
}
