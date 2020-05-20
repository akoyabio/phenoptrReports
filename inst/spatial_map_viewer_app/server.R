# Server side of Shiny app

server = function(input, output, session) {

  #### Instantiate phenotype inputs ####
  phenotype_output  =
    shiny::callModule(phenotype_color_module, 'phenotype',
                      available_phenotypes, csd)
  phenotype2_output =
    shiny::callModule(phenotype_color_module, 'phenotype2',
                      available_phenotypes, csd)

  #### Create the Save All button ####
  output$save_all_impl = shiny::renderUI({
    # If we are running locally we can save directly to the file system
    # Otherwise we need to use the Shiny download mechanism
    if (session$clientData$url_hostname=='127.0.0.1')
      shiny::actionButton('save_all_local', 'Save all', icon("download"))
    else
      shiny::downloadButton('save_all', 'Save all')
  })

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
  plot_and_data = reactive(label='plot_and_data', {
    check_for_changes()
    if (is_touching())
      return (list(plot=NULL, data=NULL))

    pheno1 = phenotype_output()$phenotype
    pheno2 = phenotype2_output()$phenotype
    shiny::isolate(update_from_to(pheno1, pheno2, input$show_as))

    phenos = phenoptrReports:::parse_phenotypes_with_na(pheno1, pheno2)
    phenoptrReports::nearest_neighbor_map(csd, input$field, .export_path,
                                          phenos,
                                          phenotype_output()$color,
                                          phenotype2_output()$color,
                                          input$show_as, input$dot_size,
                                          input$add_logo)
  })

  #### Update the from-to popup ####
  # Give it friendly labels if phenotypes are defined;
  # otherwise, make it a prompt.
  update_from_to = function(pheno1, pheno2, selected) {
    if (is.na(pheno1) || is.na(pheno2)) {
      names = rep('Please define two phenotypes', 5)
    } else {
      names = c(
        stringr::str_glue('Nearest {pheno2} to each {pheno1}'),
        stringr::str_glue('Nearest {pheno1} to each {pheno2}'),
        'Mutual nearest neighbors', 'None', 'Touching cells'
      )
    }

    choices = list('from_to', 'to_from', 'mutual', 'none', 'touching') %>%
      rlang::set_names(names)

    shiny::updateSelectInput(session, 'show_as',
                             choices=choices, selected=selected)
  }

  #### Plot output ####
  shiny::observe(label='plot_output', {
    nn = plot_and_data()
    if (is.null(nn$plot)) {
      output$plot = NULL
    } else {
      output$plot = shiny::renderPlot(nn$plot)
    }
  })

  #### Update the touching image when any of the parameters changes ####
  image_and_data = shiny::reactive(label='image_and_data', {
    check_for_changes()
    if (!is_touching())
      return (list(image=NULL, data=NULL))

    pheno1 = phenotype_output()$phenotype
    pheno2 = phenotype2_output()$phenotype
    shiny::isolate(update_from_to(pheno1, pheno2, input$show_as))

    phenos = phenoptrReports:::parse_phenotypes_with_na(pheno1, pheno2)
    shiny::req(!any(is.na(phenos)))

    tag = make_filename(input$field,
                  pheno1, pheno2,
                  input$show_as, '')
    tag = shiny::span(shiny::strong('Creating image: '), tag)
    id = shiny::showNotification(tag, duration=NULL, closeButton=FALSE)

    # Capture and report errors from count_touching_cells_fast
    safe_count = purrr::safely(phenoptr::count_touching_cells_fast,
                               otherwise=list())
    result = safe_count(csd, input$field, .export_path,
                                                 phenos,
                                                 phenotype_output()$color,
                                                 phenotype2_output()$color,
                                                 discard_dups=TRUE)
    shiny::removeNotification(id)

    if (!is.null(result$error)) {
      shiny::showModal(
        shiny::modalDialog(
          shiny::p(result$error$message), title='Processing error',
          footer=modalButton('OK'), easyClose=TRUE))
    }

    result$result
  })

  #### Image output ####
  shiny::observe(label='image_output', {
    im = image_and_data()
    if (is.null(im$image)) {
      output$touching = NULL
    } else {
      image = im$image
      output$touching = shiny::renderUI({
        # Save the image as a temporary png file
        # Note image is transposed; EBImage::writeImage will flip it
        width  = dim(image)[1]
        height = dim(image)[2]

        # A temp file to save the output.
        outfile = tempfile(fileext='.jpeg', tmpdir=temp_dir)
        out_src = paste0('image/', basename(outfile))
        EBImage::writeImage(image, outfile, quality=90)

        shiny::img(src=out_src, width=width, height=height,
                   alt='Image of touching cells')
      })
    }
  })

  #### Handle Save ####
  output$save_plot = shiny::downloadHandler(
    filename = function() {
      extn = ifelse(save_plot_data(), '.zip', '.png')
      make_filename(input$field,
             phenotype_output()$phenotype,
             phenotype2_output()$phenotype,
             input$show_as, extn)
    },
    content = function(file) {
      data = if (is_touching()) image_and_data() else plot_and_data()
      if (save_plot_data()) {
        # Save both plot and data to a temp directory and make a zip file
        owd = setwd(tempdir())
        on.exit(setwd(owd))

        pheno1 = phenotype_output()$phenotype
        pheno2 = phenotype2_output()$phenotype
        image_filename = make_filename(input$field, pheno1, pheno2,
                                       input$show_as, '.png')
        save_plot_or_image(data, image_filename)

        data_filename = make_filename(input$field, pheno1, pheno2,
                                      input$show_as, '.txt')
        vroom::vroom_write(data$data, data_filename, na='#N/A')

        # Create the zip file
        zip::zipr(file, c(image_filename, data_filename))
      } else {
        # Just the plot
        save_plot_or_image(data, file)

      }
    }
  )

  # Do we have plot data and have we been asked to save it?
  save_plot_data = function() {
    input$save_data &&
      ((is_touching() && !is.null(image_and_data()$data))
       ||
      (!is_touching() && !is.null(plot_and_data()$data))
      )
  }

  is_touching = shiny::reactive({ input$show_as == 'touching' })

  #### Handle Save All - remote version ####
  # Save All code for when the client browser is not running on the same machine
  # as the server. To save all, we have to save in a temp directory, make a zip
  # file and download the zip via the browser.
  output$save_all = shiny::downloadHandler(
    filename = function() {
      make_filename(basename(.export_path),
                    phenotype_output()$phenotype,
                    phenotype2_output()$phenotype,
                    input$show_as, ".zip")
    },

    content = function(file) {
      # Write to a temp dir to avoid permission issues
      owd = setwd(tempdir())
      on.exit(setwd(owd))

      shiny::withProgress(message='Creating image files', value=0, {
        files = write_all_files()

        # Create the zip file
        shiny::setProgress(1, detail='Writing zip file')
        zip::zipr(file, files)
      })
    }
  )

  #### Handle Save All - local version ####
  # Save All code for when the client browser is on the server machine
  # We can save directly to the local file system
    observeEvent(input$save_all_local, {
      # Create a directory in the user's Downloads directory
      download_base = fs::path_home('Downloads')
      output_dir = file.path(download_base, basename(.export_path))
      dir.create(output_dir, recursive=TRUE)
      owd = setwd(output_dir)
      on.exit(setwd(owd))

      shiny::withProgress(message='Saving image files', value=0, {
        files = write_all_files()
      })
    }
  )

  #### Write all files ####
  # Write all image files and (optionally) data files to the
  # current working directory. Return a list of file names written.
  write_all_files = function() {
    files = NULL;

    pheno1 = validate_candidate(phenotype_output()$phenotype)
    pheno2 = validate_candidate(phenotype2_output()$phenotype)
    color1 = phenotype_output()$color
    color2 = phenotype2_output()$color
    show_as = input$show_as
    dot_size = input$dot_size
    add_logo = input$add_logo

    phenos = phenoptrReports:::parse_phenotypes_with_na(pheno1, pheno2)

    # Number of progress messages
    n_progress = length(available_fields) + 1

    # Combine all data to a single table
    field_data = tibble::tibble()

    # Loop through the fields
    for (field in available_fields) {
      shiny::incProgress(1/n_progress, detail=field)

      data = if (is_touching()) {
        phenoptr::count_touching_cells_fast(csd, field, .export_path,
                                            phenos, color1, color2,
                                            discard_dups=TRUE)
      } else {
        phenoptrReports::nearest_neighbor_map(csd, field, .export_path,
                                              phenos, color1, color2,
                                              show_as, dot_size, add_logo)
      }

      # Write the plot to a file, save the name
      filename = make_filename(field, pheno1, pheno2, show_as, '.png')
      save_plot_or_image(data, filename)
      files = c(filename, files)

      # Remember the data
      field_data = dplyr::bind_rows(field_data, data$data)
    }

    # Optionally save the data
    if (input$save_data) {
      filename = make_filename(basename(.export_path),
                               pheno1, pheno2, show_as, '.txt')
      vroom::vroom_write(field_data, filename, na='#N/A')
      files = c(filename, files)
    }

    return(files)
  }

  #### Helper functions ####
  # Check for significant changes in the input parameters and
  # remember last state. If there are no changes, this does not return.
  # This is a reactive so its value can be tested more than once per
  # input change and it will give the same value each time.
  check_for_changes = shiny::reactive({
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
  })

  # Validate candidate phenotypes
  validate_candidate = function(candidate) {
    if (shiny::isTruthy(candidate) &&
        phenoptr::validate_phenotype_definitions(
          candidate, available_phenotypes, csd)=='')
      candidate else NA
  }

  # Make a descriptive file name for the download
  make_filename = function(field, pheno1, pheno2, show_as, extn) {
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
                    none = stringr::str_glue('_{pheno2}_{pheno1}'),
                    touching = stringr::str_glue('_{pheno1}_touch_{pheno2}')
      )
      name = paste0(name, suffix)
    }


    name = paste0(name, extn)

    # Remove unallowed characters
    illegal = "[/\\?<>\\:*|\":]" # From fs::path_sanitize
    name = stringr::str_replace_all(name, illegal, '_')
    name
  }

  # Save a ggplot object or EBImage image, depending on what we are given
  save_plot_or_image = function(data, file) {
    if ('plot' %in% names(data)) {
      save_plot(data$plot, file)
    } else {
      save_image(data$image, file)
    }
  }

  save_plot = function(p, file) {
    ggplot2::ggsave(file, plot=p, device = "png", width=11, height=11)
  }

  save_image = function(image, file) {
    EBImage::writeImage(image, file)
  }

  # Clean up and stop the server when the user closes the app window
  session$onSessionEnded(function() {
    shiny::stopApp()

    # Clean up our temp dir
    shiny::removeResourcePath('image')
    # Does unlink remove files in temp_dir? The docs are ambiguous.
    # Let's make sure
    list.files(temp_dir, full.names=TRUE) %>% purrr::walk(file.remove)
    unlink(temp_dir, recursive=TRUE)
  })

}

server
