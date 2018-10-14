# Phenotype module allows input of a phenotype definition
# and optional expression column.
phenotype_module_ui = function(id, values, show_help=TRUE) {
  ns = NS(id)

  choices = setNames(c(NA, values), c('None', values))

  # Build the input row with optional help button
  input_row = list(
    shiny::column(5, shiny::textInput(ns("phenotype"), 'Phenotype:',
                                      placeholder='Phenotype definition')),
    shiny::column(5, shiny::selectInput(ns("expression"), 'Expression:',
                                        choices=choices,
                                        selected='None')))
  if (show_help)
    input_row = c(input_row, list(
      shiny::column(1, style='padding-top: 23px;',
                  shiny::actionButton(ns('help'), '',
                         style='border: none; background: transparent',
                         icon=shiny::icon('question-circle-o', class='fa-2x')))

    ))

  shiny::tagList(shiny::fluidRow(shiny::tagList(input_row)),
    shiny::strong(shiny::textOutput(ns('error')),
                  style='color: maroon')
  )
}

phenotype_module = function(input, output, session, phenotypes) {
  # Check for valid phenotype definitions
  observe({
    req(input$phenotype)
    output$error = renderText(
      validate_phenotype_definitions(input$phenotype, phenotypes))
  })

  # Show help
  observeEvent(input$help, {
    req(input$help)
    shiny::showModal(
      shiny::modalDialog(
        shiny::p('Phenotype definitions are composed from the names of phenotypes. Definitions must end in + or -, for example CD3+ or CD8-.'),
        shiny::p('Individual phenotypes can be combined with slash (/) or comma (,).'),
        shiny::p('Combine with a slash to define double positive phenotypes, for example CD3+/CD8+.'),
        shiny::p('Combine with a comma to allow either phenotype, for example CD68+,CD163+.'),
        shiny::p(shiny::a('Online Help',
                          href="https://akoyabio.github.io/phenoptrReports/articles/analysis.html")),
        title = 'Defining phenotypes',
        footer=NULL, easyClose = TRUE, fade = FALSE))
  })

  return(reactive({
    list(phenotype=input$phenotype,
         expression=input$expression)
  }))
}

# Check that pheno can be formed from available phenotypes.
# Returns an error message or empty string
validate_phenotype_definitions = function(pheno, available) {
  if (is.null(pheno) || pheno==''
      || stringr::str_detect(pheno, 'Total|All'))
    return('')

  phenos = stringr::str_split(pheno, '[,/]')[[1]] %>%
    stringr::str_trim()

  if (!all(stringr::str_detect(phenos, '[+-]$')))
    return('Phenotype definitions must end with + or -.')

  phenos = stringr::str_remove(phenos, '[+-]$')
  missing = !phenos %in% available
  if (any(missing))
    return(paste0('Unknown phenotype(s): ', paste(phenos[missing], sep=', ')))

  return('')
}

phenotype_module_test = function() {
  choices = c('Nucleaus PDL-1 Mean', 'Membrane PDL-1 Mean')
  ui = shiny::fluidPage('Test',
                        shiny::wellPanel(phenotype_module_ui('test1', choices),
                        shiny::br(),
                        phenotype_module_ui('test2', choices, show_help=FALSE)),
                        shiny::p('Results',
                                  shiny::textOutput('results'))
  )

  server = function(input, output, session) {
    available = c('CD3', 'CD8')
    the_data1 = shiny::callModule(phenotype_module, 'test1', available)
    the_data2 = shiny::callModule(phenotype_module, 'test2', available)

    observe({
      output$results = shiny::renderText(paste(the_data1(), collapse='\n'))
      print(text)})
  }

  shiny::shinyApp(ui, server)
}

#shiny::runApp(phenotype_module_test())
