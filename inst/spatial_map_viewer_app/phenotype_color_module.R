# Phenotype module allows input of a phenotype definition and color.
# It validates the definition against a list of available phenotypes
# and optionally validates any expressions against a cell seg data frame.

# CSS must be included by the main UI module
phenotype_color_module_css = '
.phenotype .form-group {
  margin-bottom: 0px;
}
.phenotype .col-sm-7 {
  padding-right: 0px;
}
.phenotype .col-sm-5 {
  padding-left: 0px;
}'

#' UI portion of the phenotype/color module
#' @param id ID for the server to use to refer to this instance
#' @param label The user-visible label for the phenotype field
#' @param value The initial color
phenotype_color_module_ui = function(id, label='Phenotype:', value='white') {
  ns = shiny::NS(id)

  shiny::tagList(shiny::fluidRow(class='phenotype',
    shiny::column(7, shiny::textInput(ns("phenotype"), label,
                                      placeholder='Phenotype definition')),
    shiny::column(5, colourpicker::colourInput(ns('color'), 'Color:',
                                               value=value,
                                               showColour='background',
                                               palette='limited'))
    ),
    shiny::strong(shiny::textOutput(ns('error')),
                    style='color: #c12525')
  )
}

#' Server portion of the phenotype/color module
#' @param input,output,session Standard parameters for Shiny modules
#' @param phenotypes The available single phenotypes.
#' @param csd A cell seg data table; if provided, it will be used to validate
#' any expressions used in phenotype definitions.
#' @param allow_multiple Are multiple phenotypes allowed?
phenotype_color_module = function(input, output, session, phenotypes,
                                  csd=NULL, allow_multiple=TRUE) {
  # Check for valid phenotype definitions
  valid = shiny::reactiveVal(FALSE)
  shiny::observe({
    if (!allow_multiple && stringr::str_detect(input$phenotype, '[,/]'))
      msg = 'Multiple phenotypes are not supported.'
    else
      msg = phenoptr::validate_phenotype_definitions(
        input$phenotype, phenotypes, csd)
    valid(msg == '' && input$phenotype != '')
    output$error = shiny::renderText(msg)
  })

    return(shiny::reactive(label=session$ns('output'), {
    list(phenotype=ifelse(valid(), input$phenotype, NA),
         color=input$color)
  }))
}


phenotype_color_module_test = function() {
  ui = shiny::fluidPage('Test',
                        shiny::wellPanel(phenotype_color_module_ui('test1'),
                        shiny::br(),
                        phenotype_color_module_ui('test2')),
                        shiny::p('Results',
                                  shiny::textOutput('results'))
  )

  server = function(input, output, session) {
    available = c('CD3', 'CD8')
    the_data1 = shiny::callModule(phenotype_color_module, 'test1', available)
    the_data2 = shiny::callModule(phenotype_color_module, 'test2', available)

    observe({
      output$results = shiny::renderText(paste(the_data1(), collapse='\n'))
      print(text)})
  }

  shiny::shinyApp(ui, server)
}

#shiny::runApp(phenotype_color_module_test())
