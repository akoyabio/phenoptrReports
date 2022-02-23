# Shiny web application to explore an annotation file

# This is a simple viewer for GeoJSON annotation files exported by Phenochart
# It is an example of how to read, filter and display annotations
# and may be useful in its own right.

library(tidyverse)
library(phenoptr)
library(sf)
library(shiny)

ui <- fluidPage(

  # Application title
  titlePanel("GeoJSON Annotation Viewer"),

  sidebarLayout(
    sidebarPanel(
      HTML('Select a GeoJSON file containing annotations:&nbsp;&nbsp;'),
      actionButton('browse', 'Browse'),
      br(), br(),

      HTML('(Optional) Select a cell seg data file:&nbsp;&nbsp;'),
      actionButton('browse_csd', 'Browse'),
      br(), br(),

      selectInput('color_by', 'Color by',
                  choices=c('Status', 'Tags')),
      selectInput('tag_filter', 'Tags to show', multiple=TRUE, choices=NULL),
      selectInput('status_filter', 'Status to show', multiple=TRUE, choices=NULL)
    ),

    # Show a plot of the selected annotations
    mainPanel(
     plotOutput("annoPlot", height='600px'),
     br(), hr(), br(),
     tableOutput('table')
    )
  )
)

server <- function(input, output, session) {
  # Raw data in a simple features data frame with columns for
  # geometry, object_type, status and tags
  the_data = reactiveVal()

  # Cell seg data
  csd = reactiveVal()

  default_dir = reactiveVal('')

  # Browse for a GeoJSON file and read it to the_data
  shiny::observeEvent(input$browse, {
    shiny::req(input$browse)

    default = default_dir()
    if (default != '') default = paste0(default, '\\*.geojson')

    path = phenoptrReports::choose_files(
      default=default,
      caption='Choose a GeoJSON file',
      filters = c("GeoJSON files (*.geojson)", "*.geojson"),
      multi=FALSE)

    # Read the data
    shiny::req(path)
    path = normalizePath(path, winslash='/', mustWork=FALSE)
    df = st_read(path)
    st_crs(df) = NA_crs_ # GeoJSON is read as WGS84; we are not that

    # Empty tags make trouble with selectInput, replace them here with '(None)'
    df$tags[df$tags==''] = '(None)'
    the_data(df)

    # Update default path
    default_dir(dirname(path))
  })

  # Browse for a cell seg data file and read it to csd
  shiny::observeEvent(input$browse_csd, {
    shiny::req(input$browse_csd)

    default = default_dir()
    if (default != '') default = paste0(default, '\\*.txt')

    path = phenoptrReports::choose_files(
      default=default,
      caption='Choose a cell seg data file',
      filters = c("Text files (*.txt)", "*.txt"),
      multi=FALSE)

    # Read the data
    shiny::req(path)
    path = normalizePath(path, winslash='/', mustWork=FALSE)
    df = read_tsv(path, na='#N/A',
                  col_select=c('Cell X Position', 'Cell Y Position'))
    csd(df)

    # Update default path
    default_dir(dirname(path))
  })

  # Update the filter inputs when the data changes
  observe({
    req(the_data())

    tags = the_data()$tags %>% unique() %>% sort()
    updateSelectInput(session, 'tag_filter', choices=tags)

    statuses = the_data()$status %>% unique() %>% sort()
    updateSelectInput(session, 'status_filter', choices=statuses)
  })

  # Filter the_data when the filter inputs change
  filtered_data = reactive({
    req(the_data())
    filtered = the_data()

    # NULL input == nothing selected == don't filter
    if (!is.null(input$tag_filter))
      filtered = filtered %>% filter(tags %in% input$tag_filter)

    if (!is.null(input$status_filter))
      filtered = filtered %>% filter(status %in% input$status_filter)
    filtered
  })

  # Plot limits - a list containing xlim and ylim
  limits = reactive({
    req(the_data())

    bbox = st_bbox(the_data())
    xlim = c(bbox['xmin'], bbox['xmax'])
    ylim = c(-bbox['ymax'], -bbox['ymin'])
    list(xlim=xlim, ylim=ylim)
  })

  # Plot output
  output$annoPlot <- renderPlot({
    req(filtered_data())
    make_plot(filtered_data(), input$color_by, limits(), csd())
})

  output$table <- renderTable(
    table(filtered_data()$tags, filtered_data()$status) %>%
      as.data.frame.matrix(),
    rownames = TRUE
  )

  # Stop the server when the user closes the app window
  session$onSessionEnded(function() {
    shiny::stopApp()
  })
}

# Helper to actually make the plot
# @param anno_data Simple features data frame to draw
# @param color_by Color by 'Status' or 'Tags'
# @param limits List containing xlim and ylim
# @param csd Optional cell data to draw
# @return A ggplot object
make_plot = function(anno_data, color_by, limits, csd=NULL) {
  # What aesthetic do we color by?
  aesthetic = switch(color_by,
                     Status=aes(color=status),
                     Tags=aes(color=tags))

  # Make a plot
  p = ggplot()

  if (!is.null(csd))
    p = p + geom_point(data=slice_sample(csd, n=10000),
                       aes(`Cell X Position`, -`Cell Y Position`),
                       shape='.', alpha=0.5)
  p + geom_sf(data=anno_data, aesthetic, size=1, fill=NA, stat='sf_invert') +
    scale_x_continuous(limits=limits$xlim) +
    scale_sf_invert(limits=limits$ylim) +
    scale_color_discrete(color_by) +
    labs(x='X', y='Y') +
    theme_minimal()
}

shinyApp(ui, server)
