# This is an RStudio Addin that provides a GUI frontend
# to mean_of_top_and_bottom_cells.

#' Generate a "Mean of top and bottom cells" report for a
#' selected merge data file and configuration file.
#'
#' `addin_100_mean_of_top_and_bottom_cells` opens a GUI that allows you to select
#' a merge data file containing results of a manufacturing staining run,
#' and a configuration file defining the columns of interest.
#' It then creates an Excel workbook and Word document reporting on the data.
#' @export
addin_100_mean_of_top_and_bottom_cells = function() {
  intro <- shiny::tagList(shiny::p(
    'This app reads a merge data file containing results',
    'of a QC staining run',
    'and creates a report on the data.'
  ))

  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
                                    .well {
                                    padding-top: 10px;
                                    padding-bottom: 5px;
                                    }
                                    h3 { margin-top: 10px; }
                                    "))),
    miniUI::gadgetTitleBar("Mean of top and bottom cells",
      right=miniUI::miniTitleBarButton('done', 'Create Report', primary=TRUE)),

    miniUI::miniContentPanel(
      intro,

      shiny::wellPanel(
        shiny::h3('Select Merge Data'),
        'Click the "Browse" button to select a merge data file',
        'containing information about the samples.',
        shiny::br(), shiny::br(),

        shiny::actionButton('browse', 'Browse...'),
        shiny::br(), shiny::br(),
        shiny::textOutput('merge_file')
      ),

      # This will be the tissue / aggregation selection panel
      shiny::uiOutput('options'),

      shiny::wellPanel(
        shiny::h3('Select Configuration File'),
        'Click the "Browse" button to select a configuration file',
        'containing the names of the columns of interest, one per line.',
        shiny::br(), shiny::br(),

        shiny::actionButton('browse_config', 'Browse...'),
        shiny::br(), shiny::br(),
        shiny::textOutput('config_file'),
        shiny::numericInput('adjacent_max', 'Maximum ratio of adjacent fluors',
                            10)
      ),

      shiny::h4(shiny::textOutput('error'), style='color: maroon')
    )
      )

  server <- function(input, output, session) {
    merge_file = shiny::reactiveVal()
    config_file = shiny::reactiveVal()

    # Handle the browse button by selecting a data file
    shiny::observeEvent(input$browse, {
      shiny::req(input$browse)
      merge_file(utils::choose.files(
        caption='Select a merge data file',
        multi=FALSE, filters = utils::Filters['txt',]
      ))

      output$merge_file = shiny::renderText(merge_file())
      set_error_text()
    })

    # Create / update the tissue category / summarize by section
    shiny::observe({
      if (!shiny::isTruthy(merge_file())) {
        output$options = shiny::renderUI(NULL);
      } else {
        # We have to read enough of the merge file to find the field column
        # name and the list of tissue categories
        merge_path = merge_file()
        merge_names =
          vroom::vroom(merge_path, n_max=1, show_col_types=FALSE)
        field_col = phenoptr::field_column(merge_names)

        # Maybe make a tissue category selection widget
        if ('Tissue Category' %in% names(merge_names)) {
          tissue_categories =
            vroom::vroom(merge_path, show_col_types=FALSE,
                         col_select='Tissue Category', na='#N/A') %>%
              `[[`('Tissue Category') %>%
              unique() %>%
              sort()
          tissue_select =
            shiny::column(6, shiny::checkboxGroupInput(
                            'tissue_categories', 'Select tissue categories:',
                            choices=tissue_categories,
                            selected = tissue_categories,
                            inline=TRUE))
        } else tissue_select = NULL

        # Make an "aggregate by" widget
        by_choices = c('Slide ID', field_col)
        agg_select = shiny::column(6,
                      shiny::selectInput('by', 'Summarize by',
                                         by_choices, selected=by_choices[1]))
        output$options = shiny::renderUI(shiny::fluidRow(tissue_select, agg_select))
      }
    })

    # Handle the browse config button by selecting a config file
    shiny::observeEvent(input$browse_config, {
      shiny::req(input$browse_config)
      config_file(utils::choose.files(
        caption='Select a configuration file',
        multi=FALSE, filters = utils::Filters['txt',]
      ))

      output$config_file = shiny::renderText(config_file())
      set_error_text()
    })

    # Handle the done button by processing files or showing an error
    shiny::observeEvent(input$done, {
      error_text = get_error_text()

      if (error_text == '') {
        shiny::showNotification('Processing, please wait!', duration=NULL,
                                closeButton=FALSE, type='message')

        # Make nice strings for reporting
        merge_path = normalizePath(merge_file(), winslash='/')
        config_path = normalizePath(config_file(), winslash='/')
        tissue_cat_str = dplyr::if_else(is.null(input$tissue_categories),
                  "NULL", deparse(input$tissue_categories))
        cmd = stringr::str_glue(
          'phenoptrReports::mean_of_top_and_bottom_cells_report(\n',
          '  merge_file="{merge_path}",\n',
          '  config_file="{config_path}",\n',
          '  by="{input$by}",\n',
          '  tissue_categories={tissue_cat_str},\n',
          '  adjacent_max={input$adjacent_max})')
        message('Running command\n', cmd)

        mean_of_top_and_bottom_cells_report(
          merge_path, config_path, input$by, input$tissue_categories,
          input$adjacent_max)

        shiny::stopApp()
      } else {
        shiny::showNotification(error_text, type='message')
      }
    })

    # Handle the cancel button by quitting
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

    # Set error message in response to user input
    # For some reason this doesn't work as a reactive so just make
    # it a function and call as needed
    set_error_text = function() {
      output$error = shiny::renderText(get_error_text())
    }

    get_error_text = function() {
      if (is.null(merge_file())) {
        'Please select a merge cell seg data file.'
      } else if (is.null(config_file())) {
        'Please select a configuration file.'
      } else ''
    }

    # Initialize
    output$error =
      shiny::renderText('Please select a merge cell seg data file.')
  }

  # Run the gadget in a dialog
  viewer <- shiny::dialogViewer(
    'Create mean of top and bottom cells report',
    width=900, height=700)
  shiny::runGadget(ui, server, viewer = viewer)
}
