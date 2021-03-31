# Server (and dynamic UI) for analysis_app
shinyServer(function(input, output, server) {

  #### Data ####
  # Data container for all fields needed by the formatters. Will contain
  # by - Field to aggregate by
  # available_phenotypes - Base names (no +/-) of all available phenotypes
  # expression_columns - Names of mean expression columns
  # field_col - Name of the column that distinguishes fields
  # phenotype_modules - A list of phenotype_module
  # slide_id_prefix - Slide ID prefix to remove
  # tissue_categories - User-selected tissue categories
  # use_regex - Is slide_id_prefix a regular expression?
  # include_nearest - Include nearest neighbor summary?
  # include_count_within - Include "count within radius" summary?
  # include_distance_details - Write a detail table for distance metrics?
  # radii - For count_within, if selected
  the_data = reactiveValues()

  # File selection
  # file_data may contain input_path, summary_path, score_path, output_dir
  # It is an ordinary list of reactive objects
  file_data = shiny::callModule(files_module, 'files')

  # Consolidated data for the formatter
  # all_data() should not contain any reactive objects
  # This allows making test values for formatter
  all_data = reactive({
    ad = c(shiny::reactiveValuesToList(the_data), purrr::map(file_data, ~.()))
    ad$phenotype_values = purrr::map(ad$phenotype_modules, function(ph) ph())
    ad$phenotype_modules = NULL
    ad
  })

  #### Read the primary cell seg data file ####
  csd = reactive({
    shiny::req(file_data$input_path())

    shiny::showModal(
      shiny::modalDialog(
        shiny::p('Reading input data...'), title='Please wait', footer=NULL))

    # We don't need all columns here, we are looking for
    # unique tissue categories, expression columns, phenotypes and samples
    d = phenoptr::read_cell_seg_data(file_data$input_path(),
                                     col_select='phenoptrReports')

    shiny::removeModal()
    d
  })

  ##### Initialize the Analysis tab ####
  shiny::observe({
    # Remove any existing output panels
    wells = c('#well1', '#well2', '#well3')
    wells %>% purrr::walk(~purrr::safely(shiny::removeUI)(.x))

    d = csd()
    tissue_categories = unique(d$`Tissue Category`)
    the_data$expression_columns = stringr::str_subset(names(d), 'Mean$')
    the_data$field_col = phenoptr::field_column(d)

    the_data$available_phenotypes = available_phenotypes = names(d) %>%
      stringr::str_subset('Phenotype ') %>%
      stringr::str_remove('Phenotype ')

    # Slide ID prefix and aggregation choices
    if ('Slide ID' %in% names(d)) {
      the_data$slide_id_prefix = slide_id_prefix =
        find_common_prefix(unique(d$`Slide ID`))
      by_choices = c('Slide ID', phenoptr::field_column(d))
    } else {
      # No Slide ID column
      the_data$slide_id_prefix = slide_id_prefix = ''
      by_choices = phenoptr::field_column(d)
    }

    the_data$by = by_choices[1] # Default

    #### Create the initial GUI with tissue and phenotype selectors ####
    new_ui = shiny::tagList(
      # First well panel has miscellaneous inputs
      shiny::div(id='well1', shiny::wellPanel(
        shiny::fluidRow(
          shiny::column(6,
            shiny::checkboxGroupInput(
              'tissue_categories', 'Select tissue categories:',
              choices=tissue_categories,
              selected = if (length(tissue_categories)==1) # Can't use ifelse
                                tissue_categories else NULL,
              inline=TRUE)),
          shiny::column(6,
            shiny::selectInput('by', 'Summarize by',
                               by_choices, selected=by_choices[1]))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput('slide_id_prefix',
                           'Slide ID prefix (will be removed)',
                           slide_id_prefix)),
          shiny::column(3, style='padding-top: 20px;',
                        shiny::checkboxInput('use_regex',
                                               'Use regular expressions',
                                               FALSE))
        )
      )),

      # Second well panel holds phenotype modules.
      # Start it with one phenotype module and a button to add more.
      shiny::div(id='well2', shiny::wellPanel(
        paste('Available phenotypes:',
              paste(available_phenotypes, collapse=', ')),
        phenotype_module_ui('pheno0', the_data$expression_columns,
                            show_score = !is.null(file_data$score_path()),
                            show_help=TRUE),
        shiny::actionButton('add', 'Add another phenotype')
      )),

      # Third well panel holds options for spatial processing
      shiny::div(id='well3', shiny::wellPanel(
        shiny::fluidRow(
          shiny::column(4, shiny::checkboxInput('include_nearest',
                             label='Include nearest neighbor summary')),
          shiny::column(4, shiny::checkboxInput('include_count_within',
                             label='Include "count within radius" summary')),
          shiny::column(4, shiny::checkboxInput('include_distance_details',
                         label='Save nearest neighbor / count within details'))
        ),
          shiny::textInput('radii', value='15',
            label='Radius or radii for "count within" (in microns, separate with comma or space)')
      ))
    )

    shiny::insertUI('#placeholder', 'afterBegin', new_ui)

    # Remember the phenotype selector
    the_data$phenotype_modules =
      list(shiny::callModule(phenotype_module, 'pheno0',
                             available_phenotypes, d))

  })

  #### Misc inputs ####
  # Handle by
  shiny::observe({
    shiny::req(input$by)
    the_data$by = input$by
  })

  # Handle changes to Slide ID prefix by saving values
  shiny::observe({
    the_data$slide_id_prefix = input$slide_id_prefix
    the_data$use_regex = input$use_regex
  })

  #### Handle Add button by adding another phenotype_module_ui ####
  shiny::observeEvent(input$add, {
    id = paste0('pheno', input$add)
    ui = phenotype_module_ui(id, the_data$expression_columns,
                             show_score = !is.null(file_data$score_path()),
                             show_help=FALSE)
    insertUI('#add', 'beforeBegin', ui)

    # Remember the result
    the_data$phenotype_modules = c(the_data$phenotype_modules,
                            list(callModule(phenotype_module, id,
                                            the_data$available_phenotypes,
                                            csd())))
  })

  #### Handle checkbox selections ####
  # Handle the nearest neighbor checkbox
  shiny::observeEvent(input$include_nearest, {
    the_data$include_nearest = shiny::isTruthy(input$include_nearest)
  })

  # Handle the count_within checkbox
  shiny::observeEvent(input$include_count_within, {
    the_data$include_count_within =
      shiny::isTruthy(input$include_count_within)
  })

  # Handle the include_distance_details checkbox
  shiny::observeEvent(input$include_distance_details, {
    the_data$include_distance_details =
      shiny::isTruthy(input$include_distance_details)
  })

  # Handle the radii text box
  shiny::observeEvent(input$radii, {
    shiny::req(input$radii)

    # Parse out comma/space delimited numbers
    radii = phenoptrReports:::parse_comma_space_values(input$radii)
    if (length(radii) > 0 && !anyNA(radii)) {
      the_data$radii = radii
    } else {
      the_data$radii = NULL
    }
  })

  #### Update the error message ####
  shiny::observe({
    if (is.null(file_data$input_path())) {
      analysis_error = 'Please select a data file in the Files tab.'
    } else if (is.null(file_data$output_dir())) {
      analysis_error ='Please select an output directory in the Files tab.'
    } else if (length(input$tissue_categories)==0) {
      analysis_error = 'Please select tissue categories.'
    } else if ((shiny::isTruthy(the_data$include_nearest)
               || shiny::isTruthy(the_data$include_count_within))
               && length(the_data$phenotype_modules) < 2) {
      analysis_error = 'Spatial statistics require at least two phenotypes.'
    } else if (shiny::isTruthy(the_data$include_count_within)
               && !requireNamespace('rtree', quietly=TRUE)) {
      analysis_error = 'Please install the rtree package with the command "remotes::install_github(\'akoyabio/rtree\')"'
    } else if (shiny::isTruthy(the_data$include_count_within)
               && !shiny::isTruthy(the_data$radii)) {
      analysis_error =
        'Radii for "count within" must be numeric and separated by space or comma.'
    } else {
      analysis_error = ''
    }
    output$analysis_error = shiny::renderText(analysis_error)
  })

  #### Update the script text in response to user selections ####
  output$the_script = renderText({
    the_data$tissue_categories = input$tissue_categories
    format_all(all_data())
  })

  #### Process the result!! ####
  shiny::observeEvent(input$process, {
    shiny::req(input$process)
    id = shiny::showNotification('Processing, please wait!', duration=NULL,
                     closeButton=FALSE, type='message')

    # Create and save the script
    script = format_all(all_data())
    script_path = file.path(file_data$output_dir(), 'Script.R')
    write_lines(script, script_path)

    # Run the script to do some work!
    source(script_path, local=new.env(), echo=TRUE)

    shiny::removeNotification(id)
    shiny::showNotification('Done!', duration=2,
                            closeButton=FALSE, type='message')
    shiny::stopApp()
  })
})
