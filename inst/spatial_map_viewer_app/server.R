# Server side of Shiny app

server <- function(input, output, session) {

  # Instantiate the server part of the phenotype inputs
  phenotype_output  =
    shiny::callModule(phenotype_color_module, 'phenotype',
                      available_phenotypes, csd, allow_multiple=FALSE)
  phenotype2_output =
    shiny::callModule(phenotype_color_module, 'phenotype2',
                      available_phenotypes, csd, allow_multiple=FALSE)

  output$plot = renderPlot({
    # Don't do anything if we don't have valid phenotype definitions
    # This greatly reduces redrawing and churn
    candidate_phenotypes = list(
      phenotype_output()$phenotype,
      phenotype2_output()$phenotype
    )
    for (candidate in candidate_phenotypes) {
      req(isTruthy(candidate))
      req(phenoptr::validate_phenotype_definitions(
        candidate, available_phenotypes, csd)=='')
    }

    # Get the parameters as non-reactive values
    field = input$field
    pheno1 = phenotype_output()$phenotype
    pheno2 = phenotype2_output()$phenotype
    color1 = phenotype_output()$color
    color2 = phenotype2_output()$color
    show_as = input$show_as
    dot_size = input$dot_size

    nearest_neighbor_map(csd, field, .export_path,
                         pheno1, pheno2, color1, color2, show_as, dot_size)
  })

  output$save_plot = downloadHandler(
    filename = function() {
      paste0(stringr::str_remove(input$field, '.im3'),
             '_',
             phenotype_output()$phenotype,
             '_',
             phenotype2_output()$phenotype,
             '_',
             input$show_as,
             '.png')
    },
    content = function(file) {
      ggsave(file, device = "png", width=11, height=11)
    },
    contentType='image/png'
  )
}

server
