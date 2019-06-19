# Minimal UI definition here
# The files tab is defined in files_module
# The analysis tab is generated dynamically by the server
shiny::shinyUI(shiny::tagList(
  shiny::tags$head(
    # Add some space below the phenotype error messages
    shiny::tags$style(HTML("#well2 div[id$='error'] {
                 color:maroon;
                 padding-bottom: 5px;
               }")),
    favicon()),

  shiny::navbarPage("Analyze inForm data",

  # Files tab is entirely in files_module
  files_module_ui('files'),

  # Analysis tab is initially just error message text
  shiny::tabPanel('Analysis',
                  shiny::div(id='placeholder'),
                  shiny::h4(shiny::textOutput('analysis_error'),
                            style='color: maroon')),

  # Run panel shows and runs the output script
  shiny::tabPanel('Run',
                  shiny::actionButton('process', 'Create Report'),
                  shiny::br(), shiny::br(),
                  shiny::p('This script will be saved and run.',
                           shiny::HTML('&nbsp;&nbsp;'),
                           shiny::a('Online Help',
                                    href="https://akoyabio.github.io/phenoptrReports/articles/analysis.html")),
                  shiny::verbatimTextOutput('the_script')
  )

)))
