# Server side of Shiny app

server <- function(input, output, session) {

  #### Instantiate phenotype inputs ####
  phenotype_output  =
    shiny::callModule(phenotype_color_module, 'phenotype',
                      available_phenotypes, csd)
  phenotype2_output =
    shiny::callModule(phenotype_color_module, 'phenotype2',
                      available_phenotypes, csd)

  #### Cache for last values ####
  last_field = last_color1 = last_color2 = ''
  last_pheno1 = last_pheno2 = NA
  last_show_as = TRUE
  last_dot_size = 3
  last_add_logo = TRUE

  #### Handle previous and next buttons ####
  shiny::observeEvent(input$previous, label='previous', {
    shiny::req(input$previous)
    current_field = shiny::isolate(input$field)
    current_ix = which(available_fields==current_field)
    prev_ix = current_ix-1
    if (prev_ix <= 0) prev_ix = length(available_fields)
    shiny::updateSelectInput(session, 'field',
                             selected=available_fields[prev_ix])
  })

  shiny::observeEvent(input$nxt, label='next', {
    shiny::req(input$nxt) # Note 'next' is a reserved word
    current_field = shiny::isolate(input$field)
    current_ix = which(available_fields==current_field)
    next_ix = current_ix+1
    if (next_ix > length(available_fields)) next_ix = 1
    shiny::updateSelectInput(session, 'field',
                             selected=available_fields[next_ix])
  })

  #### Update the plot when any of the parameters changes ####
  the_plot = reactive(label='the_plot', {
    # Get the parameters as non-reactive values
    field = input$field
    pheno1 = validate_candidate(phenotype_output()$phenotype)
    pheno2 = validate_candidate(phenotype2_output()$phenotype)
    color1 = phenotype_output()$color
    color2 = phenotype2_output()$color
    show_as = input$show_as
    dot_size = input$dot_size
    add_logo = input$add_logo

    # Quit if no change, else save current state
    # This prevents redrawing as the user is typing a phenotype name
    # This is really ugly
    req(field != last_field ||
          is.na(pheno1) != is.na(last_pheno1) ||
          (!is.na(pheno1) && pheno1 != last_pheno1) ||
          is.na(pheno2) != is.na(last_pheno2) ||
          (!is.na(pheno2) && pheno2 != last_pheno2) ||
          color1 != last_color1 ||
          color2 != last_color2 ||
          show_as != last_show_as ||
          dot_size != last_dot_size ||
          add_logo != last_add_logo)

    last_field <<- field
    last_pheno1 <<- pheno1
    last_pheno2 <<- pheno2
    last_color1 <<- color1
    last_color2 <<- color2
    last_show_as <<- show_as
    last_dot_size <<- dot_size
    last_add_logo <<- add_logo

    shiny::isolate(update_from_to(pheno1, pheno2, input$show_as))

    phenos = phenoptrReports:::parse_phenotypes_with_na(pheno1, pheno2)
    p = phenoptrReports::nearest_neighbor_map(csd, field, .export_path,
                         phenos, color1, color2,
                         show_as, dot_size, add_logo)
  })

  # Update the from-to popup with friendly labels if phenotypes are defined;
  # otherwise, make it a prompt.
  update_from_to = function(pheno1, pheno2, selected) {
    if (is.na(pheno1) || is.na(pheno2)) {
      names = rep('Please define two phenotypes', 4)
    } else {
      names = c(
        stringr::str_glue('Nearest {pheno2} to each {pheno1}'),
        stringr::str_glue('Nearest {pheno1} to each {pheno2}'),
        'Mutual nearest neighbors', 'None'
      )
    }

    choices = list('from_to', 'to_from', 'mutual', 'none') %>%
      rlang::set_names(names)

    shiny::updateSelectInput(session, 'show_as',
                             choices=choices, selected=selected)
  }

  #### Plot output ####
  shiny::observe(label='plot_output', {
    p = the_plot()
    if (!is.null(p))
      output$plot = renderPlot(p)
  })

  #### Handle Save ####
  output$save_plot = downloadHandler(
    filename = function() {
      make_filename(input$field,
             phenotype_output()$phenotype,
             phenotype2_output()$phenotype,
             input$show_as)
    },
    content = function(file) {
      save_plot(the_plot(), file)
    },
    contentType='image/png'
  )

  #### Handle Save All ####
  # To save all, we have to save in a temp directory and then make a zip
  output$save_all <- downloadHandler(
    filename = function() {
      paste0(basename(.export_path), '_',
             phenotype_output()$phenotype, '_',
             phenotype2_output()$phenotype,".zip")

    },

    content = function(file) {
      # Write to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;

      pheno1 = validate_candidate(phenotype_output()$phenotype)
      pheno2 = validate_candidate(phenotype2_output()$phenotype)
      color1 = phenotype_output()$color
      color2 = phenotype2_output()$color
      show_as = input$show_as
      dot_size = input$dot_size
      add_logo = input$add_logo

      phenos = phenoptrReports:::parse_phenotypes_with_na(pheno1, pheno2)

      shiny::withProgress(message='Creating image files', value=0, {
        # Number of progress messages
        n_progress = length(available_fields) + 1

        # Loop through the fields
        for (field in available_fields) {
          shiny::incProgress(1/n_progress, detail=field)

          # Write each plot to a file, save the name
          p = phenoptrReports::nearest_neighbor_map(csd, field, .export_path,
                            phenos, color1, color2,
                            show_as, dot_size, add_logo)
          filename = make_filename(field, pheno1, pheno2, show_as)
          save_plot(p, filename)
          files <- c(filename,files)
        }

        # Create the zip file
        shiny::setProgress(1, detail='Writing zip file')
        zip::zipr(file,files)
      })
    }
  )

  #### Helper functions ####
  # Validate candidate phenotypes
  validate_candidate = function(candidate) {
    if (shiny::isTruthy(candidate) &&
        phenoptr::validate_phenotype_definitions(
          candidate, available_phenotypes, csd)=='')
      candidate else NA
  }

  make_filename = function(field, pheno1, pheno2, show_as) {
    name = stringr::str_remove(field, '.im3')
    if (is.na(pheno1) || is.na(pheno2)) {
      # No nearest neighbors shown, just append phenotype names
      if (!is.na(pheno1)) name = paste0(name, '_', pheno1)
      if (!is.na(pheno2)) name = paste0(name, '_', pheno2)
    } else {
      suffix = switch(show_as,
                      from_to = stringr::str_glue('_{pheno2}_near_{pheno1}'),
                      to_from = stringr::str_glue('_{pheno1}_near_{pheno2}'),
                      mutual = stringr::str_glue('_{pheno1}_{pheno2}_mutual_nn'),
                      none = stringr::str_glue('_{pheno2}_{pheno1}')
      )
      name = paste0(name, suffix)
    }

    name = paste0(name, '.png')

    # Remove unallowed characters
    illegal = "[/\\?<>\\:*|\":]" # From fs::path_sanitize
    name = stringr::str_replace_all(name, illegal, '_')
    name
  }

  save_plot = function(p, file) {
    ggsave(file, plot=p, device = "png", width=11, height=11)
  }

  # Stop the server when the user closes the app window
  session$onSessionEnded(stopApp)

}

server
