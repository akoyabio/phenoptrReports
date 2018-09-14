# Define server logic required to draw a histogram
shinyServer(function(input, output, server) {

  # Data container. Will contain
  # path - Path to consolidated file
  # expression_columns - Names of mean expression columns
  # summary_path - Path to summary file, if given
  # phenotypes - A list of phenotype_module
  # tissue_categories - User-selected tissue categories
  the_data = reactiveValues()

  # Handle click on the browse button by opening a data file
  shiny::observeEvent(input$browse, {
    shiny::req(input$browse)
    path = purrr::possibly(choose.files, otherwise=NULL)(
      caption='Select a consolidated data file',
      filters = Filters['txt',],
      multi=FALSE)

    req(path)
    output$selected_file = renderText(paste('Selected file:', path))

    # Read and validate the file
    d = purrr::possibly(readr::read_tsv, otherwise=NULL)(path)
    if (is.null(d)) {
      output$browse_error = renderText('Error reading file.')
    } else if ('Phenotype' %in% names(d) ||
               (sum(stringr::str_detect(names(d), 'Phenotype '))==0) ||
               !'Slide ID' %in% names(d) ||
               !'Tissue Category' %in% names(d)) {
      output$browse_error = renderText('Please select a consolidated data file.')
    } else {
      output$browse_error = renderText('')

      # Save the path and expression columns
      the_data$path = path
      the_data$expression_columns = stringr::str_subset(names(d), 'Mean$')

      # Remove any existing output panel
      safely(removeUI)('#well2')

      # Create the initial output panel with tissue and phenotype selectors
      tissue_categories = unique(d$`Tissue Category`)
      available_phenotypes = names(d) %>%
        stringr::str_subset('Phenotype ') %>%
        stringr::str_remove('Phenotype ') %>%
        paste(collapse=', ')

      new_ui = div(id='well2', wellPanel(
        checkboxGroupInput('tissue_categories', 'Select tissue categories:',
                           choices=tissue_categories, inline=TRUE),
        paste('Available phenotypes:', available_phenotypes),
        phenotype_module_ui('pheno0', the_data$expression_columns),
        actionButton('add', 'Add another phenotype'),
        br(), br(),
        verbatimTextOutput('results'),
        br(), br(),

        actionButton('process', 'Do It!')

      ))
      insertUI('#well1', 'afterEnd', new_ui)

      # Remember the phenotype selector
      the_data$phenotypes = list(callModule(phenotype_module, 'pheno0'))
    }
  })

  # Handle Add button by adding another phenotype_module_ui
  observeEvent(input$add, {
    id = paste0('pheno', input$add)
    ui = phenotype_module_ui(id, the_data$expression_columns)
    insertUI('#add', 'beforeBegin', ui)
    the_data$phenotypes = c(the_data$phenotypes, list(callModule(phenotype_module, id)))
  })

  # Update the output text in response to user selections
  output$results = renderText({
    the_data$tissue_categories = input$tissue_categories
    format_all(the_data)
  })

  # Process the result!!
  observeEvent(input$process, {
    req(input$process)
    script = format_all(the_data)
    script_path = file.path(dirname(the_data$path), 'Script.R')
    write_lines(script, script_path)
    source(script_path, local=new.env())
  })
})
