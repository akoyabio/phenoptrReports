intro = shiny::p('This app allows you to analyze a consolidated inForm',
                 'data file. To get started, choose the file to analyze.')

browse_panel = shiny::div(id='well1', shiny::wellPanel(
  shiny::h4('Select a consolidated data file'),

  shiny::actionButton('browse', 'Browse...'),
  shiny::br(), shiny::br(),
  shiny::textOutput('selected_file'),
  shiny::h4(shiny::textOutput('browse_error'), style='color: maroon')
#  shiny::br(), shiny::br(),

  # shiny::h4('Optional: Select a cell seg summary file'),
  # 'This will be used for density calculations.', shiny::br(),
  # shiny::actionButton('browse_summary', 'Browse...'),
  # shiny::textOutput('selected_summary')
))


shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "analysis_app.css")
  ),

  titlePanel("Analyze inForm data"),

  mainPanel(width=12,
    intro,
    browse_panel
  )

))
