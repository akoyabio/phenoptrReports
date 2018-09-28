# Minimal UI definition here
# The files tab is defined in files_module
# The analysis tab is generated dynamically by the server
shiny::shinyUI(shiny::tagList(
  # Add some space below the phenotype error messages
  shiny::tags$head(shiny::tags$style(HTML("#well2 div[id$='error'] {
               color:maroon;
               padding-bottom: 5px;
             }"))),

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
                  shiny::p('This script will be saved and run.'),
                  shiny::verbatimTextOutput('the_script')
  )

)))
