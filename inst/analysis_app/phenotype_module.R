# Phenotype module allows input of a phenotype definition
# and optional expression column.
phenotype_module_ui = function(id, values) {
  ns = NS(id)

  choices = setNames(c(NA, values), c('None', values))
  shiny::tagList(fluidRow(
    shiny::column(6, shiny::textInput(ns("phenotype"), 'Phenotype:',
                        placeholder='Phenotype definition')),
    shiny::column(6, shiny::selectInput(ns("expression"), 'Expression:',
                          choices=choices,
                          selected='None'))
    ),
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
