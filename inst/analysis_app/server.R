# Define server logic required to draw a histogram
shinyServer(function(input, output, server) {

  # Data container. Will contain
  # expression_columns - Names of mean expression columns
  # phenotypes - A list of phenotype_module
  # tissue_categories - User-selected tissue categories
  the_data = reactiveValues()

  # File selection
  # file_data may contain input_path, summary_path, score_path, output_dir
  file_data = shiny::callModule(files_module, 'files')

  # Consolidated
  all_data = reactive({
    c(reactiveValuesToList(the_data), file_data())
  })

  # Handle changes in file_data$input_path by initializing the analysis panel
  shiny::observe({
    # Remove any existing output panel
    purrr::safely(shiny::removeUI)('#well2')

    shiny::req(file_data()$input_path)
    shiny::req(file_data()$output_dir)

    # Read the data file and get some info about it
    d = phenoptr::read_cell_seg_data(file_data()$input_path)
    tissue_categories = unique(d$`Tissue Category`)
    the_data$expression_columns = stringr::str_subset(names(d), 'Mean$')
    available_phenotypes = names(d) %>%
      stringr::str_subset('Phenotype ') %>%
      stringr::str_remove('Phenotype ') %>%
      paste(collapse=', ')

    # Create the initial GUI with tissue and phenotype selectors
    new_ui = shiny::div(id='well2', shiny::wellPanel(
      shiny::checkboxGroupInput('tissue_categories', 'Select tissue categories:',
                         choices=tissue_categories, inline=TRUE),
      paste('Available phenotypes:', available_phenotypes),
      phenotype_module_ui('pheno0', the_data$expression_columns),
      shiny::actionButton('add', 'Add another phenotype'),
      shiny::br(), shiny::br(),
      shiny::verbatimTextOutput('results'),
      shiny::br(), shiny::br(),

      shiny::actionButton('process', 'Do It!')

    ))
    shiny::insertUI('#placeholder', 'afterBegin', new_ui)

    # Remember the phenotype selector
    the_data$phenotypes = list(shiny::callModule(phenotype_module, 'pheno0'))

  })

  # Handle Add button by adding another phenotype_module_ui
  observeEvent(input$add, {
    id = paste0('pheno', input$add)
    ui = phenotype_module_ui(id, the_data$expression_columns)
    insertUI('#add', 'beforeBegin', ui)
    the_data$phenotypes = c(the_data$phenotypes, list(callModule(phenotype_module, id)))
  })

  # Update the error message
  shiny::observe({
    if (is.null(file_data()$input_path)) {
      analysis_error = 'Please select a data file in the Files tab.'
    } else if (is.null(file_data()$output_dir)) {
      analysis_error ='Please select an output directory in the Files tab.'
    } else if (length(input$tissue_categories)==0) {
      analysis_error = 'Please select tissue categories.'
    } else {
      analysis_error = ''
    }
    output$analysis_error = shiny::renderText(analysis_error)
  })

  # Update the output text in response to user selections
  output$results = renderText({
    the_data$tissue_categories = input$tissue_categories
    format_all(all_data())
  })

  # Process the result!!
  observeEvent(input$process, {
    req(input$process)
    script = format_all(all_data())
    script_path = file.path(file_data()$output_dir, 'Script.R')
    write_lines(script, script_path)
    source(script_path, local=new.env())
  })
})
