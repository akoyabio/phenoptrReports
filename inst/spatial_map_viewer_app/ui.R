
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "Nearest neighbor explorer",
    titleWidth=360-42), # Magic number aligns the hamburger with the sidebar

  #### Sidebar ####
  shinydashboard::dashboardSidebar(
    width = 360,
    # Selector for phenotype
    'Available phenotypes:',
    paste(available_phenotypes, collapse=', '),
    phenotype_color_module_ui('phenotype', 'First phenotype:', 'red'),
    phenotype_color_module_ui('phenotype2', 'Second phenotype:', '#1E90FF'),
    shiny::hr(),
    shiny::selectInput('show_as', 'Show nearest neighbors as:',
                       choices=c(`Please define two phenotypes`='from_to',
                                 `Please define two phenotypes`='to_from',
                                 `Please define two phenotypes`='mutual',
                                 `Please define two phenotypes`='none',
                                 `Please define two phenotypes`='touching')),
    shiny::hr(),
    shiny::sliderInput('dot_size', 'Dot size', min=1, max=5, value=3, step=1),
    shiny::fluidRow(shiny::column(4, shiny::downloadButton('save_plot', 'Save image')),
                    shiny::column(4, shiny::downloadButton('save_all', 'Save all'))
    ),
    shiny::fluidRow(id='save-controls',
      shiny::column(6, shiny::checkboxInput('save_data', 'Save data with image')),
      shiny::column(6, shiny::checkboxInput('add_logo', 'Show logo', value=TRUE))
    )
  ),

  #### Body ####
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

        # Alignment for checkboxes
        "#save-controls {
          margin-left: 0px;
        }
        #save-controls .shiny-input-container {
          padding-right: 0px;
        }
        #save-controls .col-sm-6 {
          padding-right: 0px;
        }",

        # Make touching cells images fit
        "#touching img {
          max-width:100%;
          height: auto;
        }",
        # CSS for the phenotype module
        phenotype_color_module_css)),

      phenoptrReports:::favicon()
    ),

    shiny::fluidRow(
      shiny::column(2, shiny::actionButton('previous', 'Previous',
                                           class='prev-next')),
      shiny::column(5, shiny::selectInput('field', 'Field:',
                                           choices=available_fields)),
      shiny::column(2, shiny::actionButton('nxt', 'Next',
                                           class='prev-next'))
    ),
    shiny::conditionalPanel(condition='input.show_as !="touching"',
      shiny::plotOutput('plot', height='800px')
    ),
    shiny::conditionalPanel(condition='input.show_as =="touching"',
      shiny::uiOutput('touching', height='800px')
    )
  )
)
