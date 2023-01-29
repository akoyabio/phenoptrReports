batch_merge_addin <- function() {
  intro <- shiny::tagList(shiny::p(
    'This app runs a batched merging of cell segmentation data for selected ',
    'samples in a cohort of slides',
    'algorithm.'
  ))

  input_dir_intro <- shiny::tagList(
    shiny::p("Input directory must contain:"),
    shiny::tags$ul(
      shiny::tags$li("inForm cell segmentation data"),
      shiny::tags$li("inForm batch file (Batch.log)"),
      shiny::tags$li("inForm algorithm (batch_procedure.ifp)")
    )
  )

  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      .well {
        padding-top: 10px;
        padding-bottom: 5px;
      }
      h3 { margin-top: 10px; }
    "))),
    miniUI::gadgetTitleBar("Acr368 OncoSignature Scoring",
                           right = shinyjs::disabled(
                             miniUI::miniTitleBarButton('done',
                                                        'Process Files',
                                                        primary = TRUE))),

    miniUI::miniContentPanel(
      intro,

      shiny::wellPanel(
        shiny::h3('Select input directory'),
        'Click the "Browse input..." button to select an input directory.',
        shiny::br(),
        input_dir_intro,

        shiny::actionButton('browse_input', 'Browse input...'),

        shiny::br(),
        shiny::textOutput('input_dir'),

        shiny::br(),
        shiny::checkboxInput("unqualified_tma_present", "Batch contains one or more unqualified TMAs for processing", FALSE)
      ),

      shiny::wellPanel(
        shiny::h3('Select output directory'),
        'Click the "Browse output..." button to select an empty output directory.',
        shiny::br(), shiny::br(),

        shiny::actionButton('browse_output', 'Browse output...'),
        shiny::br(),
        shiny::textOutput('output_dir')
      ),

      shiny::h4(shiny::textOutput('error'), style = 'color: maroon')
    )
  )

  server <- function(input, output, session) {
    output_dir <- shiny::reactiveVal()
    input_dir <- shiny::reactiveVal()
    enable_done <- shiny::reactiveValues()
    enable_done$input_dir <- FALSE
    enable_done$output_dir <- FALSE


    # Handle the browse_input button by selecting a folder
    shiny::observeEvent(input$browse_input, {
      shiny::req(input$browse_input)
      input_dir(phenoptrReports::choose_directory(
        caption = 'Select an input directory'
      ))

      output$input_dir <- shiny::renderText(input_dir())

      enable_done$input_dir <- shiny::isTruthy(input_dir())

      if (enable_done$input_dir & enable_done$output_dir) {
        shinyjs::enable(id = 'done')
      } else {
        shinyjs::disable(id = 'done')
      }

      error_text <- get_error_text()
      set_error_text()
    })

    # Handle the browse_output button by selecting a folder
    shiny::observeEvent(input$browse_output, {
      shiny::req(input$browse_output)
      output_dir(phenoptrReports::choose_directory(
        default = dirname(input_dir()),
        caption = 'Select an output folder'
      ))

      output$output_dir <- shiny::renderText(output_dir())

      enable_done$output_dir <- shiny::isTruthy(output_dir())

      if (enable_done$input_dir & enable_done$output_dir) {
        shinyjs::enable(id = 'done')
      } else {
        shinyjs::disable(id = 'done')
      }

      error_text <- get_error_text()
      set_error_text()
    })

    # Handle the process files button
    shiny::observeEvent(input$done, {

      # disable process button while running function
      shinyjs::disable(id = 'done')

      # use shinyjs to output status message; renderText does not work in
      # this situation
      shinyjs::html("error",
                    'Processing data. Please see R console for status updates.')

      return_paths <- list(base_path = input_dir(),
                           out_path = output_dir(),
                           unqualified_tma_present = input$unqualified_tma_present)

      shiny::stopApp(return_paths)
    })

    # Handle the cancel button by quitting
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

    # Set error message in response to user input
    set_error_text <- function() {
      output$error <- shiny::renderText(get_error_text())
    }

    get_error_text <- function() {
      if (!enable_done$input_dir) {
        'Please select an input directory.'
      } else if (!enable_done$output_dir) {
        'Please select an output directory.'
      } else {
        ''
      }
    }

    # Initialize
    output$error <- shiny::renderText('Please select an input directory.')
  }

  # Run the gadget in paneViewer; running in dialogViewer causes error in script
  shiny::runGadget(ui, server, viewer = shiny::paneViewer())

}


