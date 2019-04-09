# Files module - UI and server logic to define file paths

# Widget to choose a single file with optional validation and error reporting.
browse_file_module_ui = function(id, header, button) {
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::h4(header),
    shiny::actionButton(ns('browse'), button),
    shiny::br(), shiny::br(),
    shiny::textOutput(ns('selected_file')),
    shiny::h4(shiny::textOutput(ns('error')), style='color: maroon')
  )
}

# Server function for file browser widget with optional validation.
# @param default_dir reactiveVal containing the starting directory.
# If provided, validator is a function that takes a path and returns a
# (possibly empty) error string.
browse_file_module = function(input, output, session, default_dir,
                              caption='Select a file',
                              validator=NULL) {
  the_path = shiny::reactiveVal()

  # Handle click on the browse button by opening a file
  shiny::observeEvent(input$browse, {
    default = default_dir()
    if (default != '') default = paste0(default, '\\*.txt')

    shiny::req(input$browse)
    path = phenoptrReports::choose_files(
      default=default,
      caption=caption,
      filters = c("Text files (*.txt)", "*.txt"),
      multi=FALSE)

    shiny::req(path)
    path = normalizePath(path, winslash='/', mustWork=FALSE)
    output$selected_file = shiny::renderText(paste('Selected file:', path))

    # Update default path
    default_dir(dirname(path))

    if (is.null(validator)) {
      error = ''
    } else {
      error = validator(path)
    }
    output$error = shiny::renderText(error)

    if (error=='') the_path(path) else the_path(NULL)
  })

  the_path
}

files_module_ui = function(id) {
  ns = shiny::NS(id)

  intro = shiny::tagList(
    shiny::p('Use this panel to select input files and an output directory.',
    shiny::HTML('&nbsp;&nbsp;'),
    shiny::a('Online Help',
      href="https://akoyabio.github.io/phenoptrReports/articles/analysis.html")))
  shiny::tabPanel(
    'Files',
     intro,
     shiny::div(id='well_input', shiny::wellPanel(
       browse_file_module_ui(ns('data_file'),
                             'Select a consolidated data file', 'Browse...'),
       shiny::hr(),

       shiny::h4('Select the output directory'),
       shiny::br(),

       shiny::actionButton(ns('browse_output'), 'Browse...'),
       shiny::br(), shiny::br(),
       shiny::textOutput(ns('output_dir')),
       shiny::hr(),

       browse_file_module_ui(ns('summary_file'),
         'Optional: Select a cell seg summary file to compute cell densities.',
         'Browse...'),
       shiny::hr(),

       browse_file_module_ui(ns('score_file'),
         'Optional: Select a scoring file to compute H-Score.',
         'Browse...')
     ))
  )
}

files_module = function(input, output, session) {
  # Data container. Will contain
  # input_path - Path to consolidated file
  # summary_path - Path to summary file, if given
  # score_path - Path to score file, if given
  # output_dir - Path to directory for results

  output_dir = reactiveVal()
  default_dir = reactiveVal('')

  input_path = shiny::callModule(browse_file_module, 'data_file',
     default_dir, caption='Select a consolidated data file',
     validator =function(path) {
       # Read and validate the file. We just need the header here
       d = purrr::possibly(readr::read_tsv, otherwise=NULL)(path, n_max=5)
       if (is.null(d)) {
         browse_error = 'Error reading file.'
       } else if ('Phenotype' %in% names(d) ||
                  (sum(stringr::str_detect(names(d), 'Phenotype '))==0)) {
         browse_error = 'Please select a consolidated data file.'
       }else if (!'Slide ID' %in% names(d) ||
                 !'Tissue Category' %in% names(d)) {
         browse_error = 'This analysis requires a data file with "Slide ID" and "Tissue Category" columns.'
       } else {
         browse_error = ''
       }
       browse_error
     })

  summary_path = shiny::callModule(browse_file_module, 'summary_file',
    default_dir, caption='Select a summary data file',
    validator =function(path) {
      if (!endsWith(path, 'cell_seg_data_summary.txt')) {
        'Please select a cell seg summary file.'
      } else ''
    })


  score_path = shiny::callModule(browse_file_module, 'score_file',
    default_dir, caption='Select a score data file',
    validator = function(path) {
      if (!endsWith(path, 'score_data.txt')) {
        'Please select a score data file.'
      } else ''
    })

  # Handle the browse_output button by selecting a folder
  shiny::observeEvent(input$browse_output, {
    shiny::req(input$browse_output)
    output_path = phenoptrReports::choose_directory(
      default=default_dir(),
      caption='Select an output folder'
    )

    output_path = normalizePath(output_path, winslash='/', mustWork=FALSE)
    output$output_dir = shiny::renderText(output_path)
    output_dir(output_path)
    default_dir(output_path)
  })

  # Return a list of reactives
  list(
    input_path=input_path,
    summary_path=summary_path,
    score_path=score_path,
    output_dir=output_dir
  )
}

files_module_test = function() {
  ui = shiny::navbarPage('Test', files_module_ui('test'),
                  shiny::br(), shiny::br(),
                  shiny::p('Results',
                  shiny::textOutput('results'))
  )

  server = function(input, output, session) {
    the_data = shiny::callModule(files_module, 'test')

    observe({
      text = purrr::map(the_data, ~.())
      output$results = shiny::renderText(paste(text, collapse='\n'))
      print(text)})
  }

  shiny::shinyApp(ui, server)
}

#shiny::runApp(files_module_test())

