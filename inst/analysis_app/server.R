# Define server logic required to draw a histogram
shinyServer(function(input, output, server) {

  # The data to analyze
  csd = reactiveVal()

  # Expression columns in the data
  expression_columns = reactiveVal()

  # Phenotype definitions
  phenotypes = reactiveVal()

  # Handle click on the browse button by opening a data file
  shiny::observeEvent(input$browse, {
    shiny::req(input$browse)
    path = purrr::possibly(choose.files, otherwise=NULL)(
      caption='Select a consolidated data file',
      filters = Filters['txt',],
      multi=FALSE)

    req(path)
    output$selected_file = renderText(path)

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
      # Save the value
      csd(d)

      # Remove any existing output panel
      safely(removeUI)('#well2')

      # Create the initial output panel with tissue and phenotype selectors
      tissue_categories = unique(d$`Tissue Category`)
      expression_columns(stringr::str_subset(names(d), 'Mean$'))
      new_ui = div(id='well2', wellPanel(
        checkboxGroupInput('tissue_categories', 'Select tissue categories:',
                           choices=tissue_categories, inline=TRUE),
        phenotype_module_ui('pheno0', expression_columns()),
        actionButton('add', 'Add another phenotype'),
        br(), br(),
        verbatimTextOutput('results')
      ))
      insertUI('#well1', 'afterEnd', new_ui)

      # Remember the phenotype selector
      phenotypes(list(callModule(phenotype_module, 'pheno0')))
    }
  })

  # Handle Add button by adding another phenotype_module_ui
  observeEvent(input$add, {
    id = paste0('pheno', input$add)
    ui = phenotype_module_ui(id, expression_columns())
    insertUI('#add', 'beforeBegin', ui)
    phenotypes(c(phenotypes(), list(callModule(phenotype_module, id))))
  })

  # Update the output text in response to user selections
  output$results = renderText({
    tissue_categories = input$tissue_categories
    phenotype_values = purrr::map(phenotypes(), function(ph) ph())
    paste0(
      format_tissue_categories(tissue_categories),
      format_phenotypes(phenotype_values),
      format_expression(phenotype_values))
  })


})
