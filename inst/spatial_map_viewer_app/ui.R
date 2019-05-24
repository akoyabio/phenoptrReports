
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Nearest neighbor explorer"),
  shinydashboard::dashboardSidebar(
    width = 360,
    # Selector for phenotype
    'Available phenotypes:',
    paste(available_phenotypes, collapse=', '),
    phenotype_color_module_ui('phenotype', 'First phenotype:', 'red'),
    phenotype_color_module_ui('phenotype2', 'Second phenotype:', '#1E90FF'),
    shiny::hr(),
    shiny::selectInput('show_as', 'Show nearest neighbors as:',
                       choices=c(`First -> Second`='from_to',
                                 `Second -> First`='to_from',
                                 Mutual='mutual',
                                 None='none')),
    shiny::hr(),
    shiny::sliderInput('dot_size', 'Dot size', min=1, max=5, value=3, step=1),
    shiny::downloadButton('save_plot', 'Save image')
  ),

  shinydashboard::dashboardBody(
    shiny::tags$head(
      shiny::tags$style(HTML(
        # Fixes for Save button
        ".skin-blue .sidebar a {
          color: #444;
          margin-left: 15px;
        }",
        phenotype_color_module_css))
    ),

    shiny::fluidRow(
      shiny::column(10, shiny::selectInput('field', 'Field:',
                                           choices=available_fields))
    ),
    shiny::fluidRow(
      shiny::plotOutput('plot', height='800')
    )
  )
)
