# Phenotype module allows input of a phenotype definition
# and optional expression column.
phenotype_module_ui = function(id, values) {
  ns = NS(id)

  choices = setNames(c(NA, values), c('None', values))
  fluidRow(
    column(6, textInput(ns("phenotype"), 'Phenotype:',
                        placeholder='Phenotype definition')),
    column(6, selectInput(ns("expression"), 'Expression:',
                          choices=choices,
                          selected='None'))
  )
}

phenotype_module = function(input, output, session) {
  # Nothing to do except give access to the input parameters
  return(reactive({
    list(phenotype=input$phenotype,
         expression=input$expression)
  }))
}
