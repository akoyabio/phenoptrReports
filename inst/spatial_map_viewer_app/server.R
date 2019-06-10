# Server side of Shiny app

server <- function(input, output, session) {

  # Instantiate the server part of the phenotype inputs
  phenotype_output  =
    shiny::callModule(phenotype_color_module, 'phenotype',
                      available_phenotypes, csd, allow_multiple=FALSE)
  phenotype2_output =
    shiny::callModule(phenotype_color_module, 'phenotype2',
                      available_phenotypes, csd, allow_multiple=FALSE)

  # Remember last values to avoid redrawing
  last_field = last_color1 = last_color2 = ''
  last_pheno1 = last_pheno2 = NA
  last_show_as = TRUE
  last_dot_size = 3

  observe({
    # Allow for invalid phenotypes; they will not be drawn
    validate_candidate = function(candidate) {
      if (shiny::isTruthy(candidate) &&
          phenoptr::validate_phenotype_definitions(
            candidate, available_phenotypes, csd)=='')
        candidate else NA
    }

    # Get the parameters as non-reactive values
    field = input$field
    pheno1 = validate_candidate(phenotype_output()$phenotype)
    pheno2 = validate_candidate(phenotype2_output()$phenotype)
    color1 = phenotype_output()$color
    color2 = phenotype2_output()$color
    show_as = input$show_as
    dot_size = input$dot_size

    # Quit if no change, else save current state
    # This prevents redrawing as the user is typing a phenotype name
    # This is really ugly
    req(field != last_field ||
          is.na(pheno1) != is.na(last_pheno1) ||
          is.na(pheno2) != is.na(last_pheno2) ||
          color1 != last_color1 ||
          color2 != last_color2 ||
          show_as != last_show_as ||
          dot_size != last_dot_size)

    last_field <<- field
    last_pheno1 <<- pheno1
    last_pheno2 <<- pheno2
    last_color1 <<- color1
    last_color2 <<- color2
    last_show_as <<- show_as
    last_dot_size <<- dot_size

    p = nearest_neighbor_map(csd, field, .export_path,
                         pheno1, pheno2, color1, color2, show_as, dot_size)
    output$plot = renderPlot(p)
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
