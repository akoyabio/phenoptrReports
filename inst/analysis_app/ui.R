intro = shiny::p('This app allows you to analyze a consolidated inForm',
                 'data file. To get started, choose the file to analyze.')

browse_panel = shiny::div(id='well1', shiny::wellPanel(
  shiny::h3('Select a consolidated data files'),
  'Click the "Browse" button to select a consolidated',
  'inForm cell seg data file.',
  shiny::br(), shiny::br(),

  shiny::actionButton('browse', 'Browse...'),
  shiny::h4('Selected file:'),
  shiny::textOutput('selected_file'),
  shiny::h4(shiny::textOutput('browse_error'), style='color: maroon')
))


shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "analysis_app.css")
  ),

  titlePanel("Analyze inForm data"),

  mainPanel(
    intro,
    browse_panel
  )

))
