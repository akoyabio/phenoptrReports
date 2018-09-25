# Minimal UI definition here
# The files tab is defined in files_module
# The analysis tab is generated dynamically by the server
shinyUI(shiny::navbarPage("Analyze inForm data",
  files_module_ui('files'),
  shiny::tabPanel('Analysis',
                  shiny::div(id='placeholder'),
                  shiny::h4(shiny::textOutput('analysis_error'),
                            style='color: maroon')),
  shiny::tabPanel('Run',
                  shiny::p('This script will be saved and run.'),
                  shiny::verbatimTextOutput('results'),
                  shiny::br(), shiny::br(),
                  shiny::actionButton('process', 'Do It!')
  )

))
