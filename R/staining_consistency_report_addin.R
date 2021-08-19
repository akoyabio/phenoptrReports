# This is an RStudio Addin that provides a GUI frontend
# to staining_consistency_report.Rmd.

#' Generate a "Staining consistency" report for a
#' selected merge data file.
#'
#' `addin_70_staining_consistency_report` opens a GUI that allows you to select
#' a merge data file containing results of a staining run.
#' It then creates a report on the data.
#' @export
addin_70_staining_consistency_report = function() {
  intro <- shiny::tagList(shiny::p(
    'This app reads a merge data file containing results',
    'of a staining run and summarizes the mean expression',
    'of a selected marker.'
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
    miniUI::gadgetTitleBar("Staining consistency report",
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
      shiny::wellPanel(
        shiny::h3('Select marker and compartment'),
        shiny::selectInput('marker', 'Marker: ',
                           choices=c(), selectize=FALSE),
        shiny::selectInput('compartment', 'Compartment: ',
                           choices=c(), selectize=FALSE)
      ),

      shiny::h4(shiny::textOutput('error'), style='color: maroon')
    )
  )

  server <- function(input, output, session) {
    merge_file = shiny::reactiveVal()

    # Handle the browse button by selecting a file
    # and populating the Marker and Compartment drop-downs
    shiny::observeEvent(input$browse, {
      shiny::req(input$browse)

      path = utils::choose.files(
        caption='Select a merge data file',
        multi=FALSE, filters = utils::Filters['txt',]
      )

      if (length(path) == 1) {
        merge_file(path)
        output$merge_file = shiny::renderText(path)
      }

      populate_dropdowns()
      set_error_text()
    })

    # Populate the Marker and Compartment drop-downs based
    # on the selected merge file
    populate_dropdowns = function() {
      shiny::req(merge_file())
      values = parse_merge_headers(merge_file())

      compartments = values$compartments
      default_compartment =
        ifelse('Membrane' %in% compartments, 'Membrane', compartments[[1]])
      shiny::updateSelectInput(session, 'compartment',
                               choices=compartments,
                               selected=default_compartment)

      markers = values$markers
      default_marker = stringr::str_subset(markers, 'CD20')
      if (length(default_marker) != 1)
        default_marker = markers[[1]]
      shiny::updateSelectInput(session, 'marker',
                               choices=markers,
                               selected=default_marker)
    }

    # Handle the done button by processing files or showing an error
    shiny::observeEvent(input$done, {
      error_text = get_error_text()

      if (error_text == '') {
        shiny::showNotification('Processing, please wait!', duration=NULL,
                                closeButton=FALSE, type='message')

        # Make nice string for reporting
        merge_path = normalizePath(merge_file(), winslash='/')
        cmd = stringr::str_glue(
          'phenoptrReports::staining_consistency_report(\n',
          '  merge_file="{merge_path}",\n',
          '  marker="{input$marker}",\n',
          '  compartment="{input$compartment}",\n',
          '  output_dir=NULL,)')
        message('Running command\n', cmd)

        output_file =
          staining_consistency_report(merge_file(),
                                      input$marker, input$compartment)
        utils::browseURL(output_file)

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
      } else ''
    }

    # Initialize
    output$error =
      shiny::renderText('Please select a merge cell seg data file.')
  }

  # Run the gadget in a dialog
  viewer <- shiny::dialogViewer('Create mean of top and bottom cells report')
  shiny::runGadget(ui, server, viewer = viewer)
}

#' Actually generate the staining consistency report
#' @param csd_path Path to a merge cell seg data file
#' @param marker Name of the marker to report
#' @param compartment Name of the compartment to report
#' @param output_dir (Optional) Directory for the result, default
#' is the directory containing `csd_path`.
#' @return The path to the created file.
#' @export
staining_consistency_report =
  function(csd_path, marker, compartment, output_dir=NULL) {
  rmd_path = system.file("rmd", "Staining_consistency_report.Rmd",
                         package="phenoptrReports")

  if (is.null(output_dir))
    output_dir = dirname(csd_path)

  temp_dir = file.path(output_dir, 'temp')

  # Don't complain about missing \VignetteIndexEntry
  old_opts = options(rmarkdown.html_vignette.check_title = FALSE)
  on.exit(options(old_opts))

  rmarkdown::render(rmd_path, output_dir=output_dir, quiet=TRUE,
                    intermediates_dir=temp_dir,
                    params=list(csd_path=csd_path,
                                marker=marker,
                                compartment=compartment))
  unlink(temp_dir)

  output_file = file.path(output_dir, "Staining_consistency_report.html")
  cat('Report written to ', output_file, '\n')
  output_file
}

# Parse the headers of a merge file to figure out what
# markers and compartments are available
parse_merge_headers = function(path) {
  headers = readr::read_tsv(path, n_max=1,
                            col_types=readr::cols(.default='c')) %>%
    names()

  compartments = c('Nucleus', 'Cytoplasm', 'Membrane', 'Entire Cell') %>%
    purrr::keep(~any(grepl(.x, headers)))

  # Extract marker names from the Nucleus Mean entries
  markers = stringr::str_match(headers, "^Nucleus (.*) Mean")[,2] %>%
    purrr::discard(is.na)

  list(compartments=compartments, markers=markers)
}
