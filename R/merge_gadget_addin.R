# This is an RStudio Addin that provides a GUI frontend
# to merge_and_summarize_cell_seg_data.

#' Merge and summarize cell seg data files
#' 
#' `merge_addin` opens a GUI that allows you to select multiple
#' cell seg data files. The selected files will be consolidated 
#' to a single file which is saved to an output directory
#' along with summary reports for each file.
#' @export
merge_addin = function() {
  intro <- shiny::p(
    'This app consolidates multiple inForm cell seg data files',
    'into a single merged file. The merged file will be written',
    'to disk along with summary reports for each data file.'
  )
  
  ui <- miniUI::miniPage(
     
    miniUI::gadgetTitleBar("Consolidate inForm data",
      right=miniUI::miniTitleBarButton('done', 'Process Files', primary=TRUE)),
    
    miniUI::miniContentPanel(
      intro,
      shiny::h3('Select merge data files'),
      'Click the "Browse Input" button to select one or more',
      'inForm cell seg data files to consolidate.',
      'Continue until all files are selected.',
      shiny::br(), shiny::br(),
      
      shiny::actionButton('browse_source', 'Browse Input...'),
      shiny::h4('Selected files:'),
      shiny::uiOutput('selected_files'),
      shiny::br(),
      
      shiny::h3('Select output directory'),
      'Click the "Browse Output" button to select the directory',
      'where the merged data and summary reports will be placed.',
      shiny::br(), shiny::br(),
      
      shiny::actionButton('browse_output', 'Browse output...'),
      shiny::br(),
      shiny::textOutput('output_dir'),
      shiny::br(),
      
      shiny::h4(shiny::textOutput('error'))
    )
  )
  
  server <- function(input, output, session) {
    file_list = shiny::reactiveVal()
    output_dir = shiny::reactiveVal()
    
    # Handle the browse_source button by putting up a file browser 
    # to select data files and adding the result to file_list
    shiny::observeEvent(input$browse_source, {
      shiny::req(input$browse_source)
      files = choose.files(
        default='F:\\DavidC\\*.*',
        caption='Select merge data files',
        filters = Filters['txt',])
      
      # Only allow cell_seg_data.txt files
      is_cell_seg = stringr::str_detect(files, '_cell_seg_data.txt$')
      if (!all(is_cell_seg)) {
        shiny::showNotification('Please select only cell_seg_data files!',
                                type='message')
      } else {
        # Add to file_list and update the list of files in the UI
        file_list(unique(c(file_list(), files)))
        output$selected_files = shiny::renderUI({
          do.call(tagList, 
                  purrr::map(file_list(), 
                      ~p(paste0(basename(dirname(.x)), '/', basename(.x)))))
        })
      }
      set_error_text()
    })
    
    # Handle the browse_output button by selecting a folder
    shiny::observeEvent(input$browse_output, {
      shiny::req(input$browse_output)
      output_dir(choose.dir(
        default='F:\\DavidC\\Test',
        caption='Select an output folder'
      ))
      
      output$output_dir = shiny::renderText(output_dir())
      set_error_text()
    })
    
    # Handle the done button by processing files or showing an error
    shiny::observeEvent(input$done, {
      error_text = get_error_text()
      
      if (error_text == '') {
        shiny::showNotification('Processing files, please be patient!',
                type='message', duration=NULL, closeButton=FALSE)
        phenoptrReports::merge_and_summarize_cell_seg_data(file_list(), 
                                                           output_dir())
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
      if (is.null(file_list())) {
        'Please select files to process.'
      } else if (is.null(output_dir())) {
        'Please select an output directory.'
      } else
        ''
    }
    
    # Initialize
    output$error = shiny::renderText('Please select files to process.')
  }
  
  # Run the gadget in a dialog
  viewer <- shiny::dialogViewer('Merge cell seg data files')
  shiny::runGadget(ui, server, viewer = viewer)
}