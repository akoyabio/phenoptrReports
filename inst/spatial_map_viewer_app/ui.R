
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "Nearest neighbor explorer",
    titleWidth=360-42), # Magic number aligns the hamburger with the sidebar
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
    shiny::fluidRow(shiny::downloadButton('save_plot', 'Save image'),
                    shiny::downloadButton('save_all', 'Save all'))
  ),

  shinydashboard::dashboardBody(
    shiny::tags$head(
      # CSS
      shiny::tags$style(HTML(
        # Fixes for Save button
        ".skin-blue .sidebar div a {
          color: #444;
          margin-left: 30px;
        }",

        # Align the prev_next buttons with the field dropdown
        ".prev-next {
          margin-top: 25px;
        }",

        # Wider notification box for "Save all"
        "#shiny-notification-panel {
          width: 350px;
        }",
        phenotype_color_module_css))
    ),

    shiny::fluidRow(
      shiny::column(2, shiny::actionButton('previous', 'Previous',
                                           class='prev-next')),
      shiny::column(5, shiny::selectInput('field', 'Field:',
                                           choices=available_fields)),
      shiny::column(2, shiny::actionButton('nxt', 'Next',
                                           class='prev-next'))
    ),
    shiny::fluidRow(
      shiny::plotOutput('plot', height='800')
    )
  )
)
