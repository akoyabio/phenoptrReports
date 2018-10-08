# Excel helpers
# Use openxlsx to write formatted worksheets with analysis results.

# Cell styles
percent_style = openxlsx::createStyle(numFmt='0%')
percent_style.1 = openxlsx::createStyle(numFmt='0.0%')
two_decimal_style = openxlsx::createStyle(numFmt='0.00')
integer_style = openxlsx::createStyle(numFmt='0')
bold_style = openxlsx::createStyle(textDecoration='bold')
bold_wrap_style = openxlsx::createStyle(textDecoration='bold',
                                        halign='center', wrapText=TRUE)

#' Write a summary table to an Excel workbook
#'
#' Write a table containing counts of fields per slide to
#' a sheet in an Excel workbook.
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param summary_table A data frame with columns for `Slide ID` and count.
#' @param sheet_name Optional name for the worksheet.
#' @family output functions
#' @export
write_summary_sheet = function(wb, summary_table,
                              sheet_name='Slide Summary')
{
  # This doesn't fit into the write_sheet template, make it from scratch here.
  # Make a new sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Write the table
  openxlsx::writeData(wb, sheet_name, summary_table, startRow=1,
                      headerStyle=bold_wrap_style, keepNA=TRUE)

  format_slide_id(wb, sheet_name, summary_table, first_data_row=2)

  # Wider count column
  openxlsx::setColWidths(wb, sheet_name, 2, 11)

  insert_page_breaks(wb, sheet_name, summary_table,
                     grid_spacing=1, num_title_rows=1)
}

#' Write a cell counts table to an Excel workbook
#'
#' Write a formatted cell counts table to a sheet in an Excel workbook.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param counts A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and counts, such as the output of [count_phenotypes].
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @family output functions
#' @export
write_counts_sheet = function(wb, counts,
                              sheet_name='Cell Counts',
                              sheet_title='Cell Counts per Phenotype')
{
  write_sheet(wb, counts, sheet_name, sheet_title, 3)
}

#' Write a cell percent table to an Excel workbook
#'
#' Write a formatted cell percent table to a sheet in an Excel workbook.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param percents A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and percent.
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @family output functions
#' @export
write_percents_sheet = function(wb, percents,
                              sheet_name='Cell Percents',
                              sheet_title='Percentage of Total Cells')
{
  write_sheet(wb, percents, sheet_name, sheet_title, 3)

  data_rows = 1:nrow(percents)+2
  data_cols = 3:ncol(percents)

  # Format the data columns as percent
  openxlsx::addStyle(wb, sheet_name, percent_style,
                     rows=data_rows, cols=data_cols,
                     gridExpand=TRUE, stack=TRUE)
}

#' Write a density table to an Excel workbook
#'
#' Write a formatted density table to a sheet in an Excel workbook.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param densities A data frame with columns for `Slide ID`, `Tissue Category`,
#'   `Tissue Area` and densities, such as the output of
#'   [compute_density_from_table].
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @family output functions
#' @export
write_density_sheet = function(wb, densities,
                               sheet_name='Cell Densities',
                               sheet_title='Cell Densities (cells/mm2)')
{
  write_sheet(wb, densities, sheet_name, sheet_title, 4)

  # Border to the left of the Area column
  openxlsx::addStyle(wb, sheet_name,
                     openxlsx::createStyle(border='Left'),
                     rows=3:(nrow(densities)+2), cols=3,
                     stack=TRUE)

  # Format tissue area with two decimal places, densities as integer
  data_rows = 1:nrow(densities)+2
  openxlsx::addStyle(wb, sheet_name, two_decimal_style,
                     rows=data_rows, cols=3, stack=TRUE)
  openxlsx::addStyle(wb, sheet_name, integer_style, rows=data_rows,
                     cols=4:ncol(densities),
                     gridExpand=TRUE, stack=TRUE)
}

#' Write an expression table to an Excel workbook
#'
#' Write a formatted cell expression table to a sheet in an Excel workbook.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param exprs A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and mean expression, such as the output of
#'   [compute_mean_expression_many]. Count columns are not reported.
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @importFrom magrittr %>%
#' @family output functions
#' @export
write_expression_sheet = function(wb, exprs,
                               sheet_name='Mean Expression',
                               sheet_title='Mean Expression') {

  # Subset and re-order columns; remove "(Opal xx) Mean" from names
  exprs = exprs %>%
    dplyr::select(`Slide ID`, `Tissue Category`, dplyr::everything()) %>%
    dplyr::select(-dplyr::contains('Count')) %>%
    dplyr::rename_all(remove_marker_mean)

  write_sheet(wb, exprs, sheet_name, sheet_title, 3)

  # Make the expression columns a little wider
  data_rows = 1:nrow(exprs)+2
  data_cols = 3:ncol(exprs)
  openxlsx::setColWidths(wb, sheet_name, data_cols, 14)

  # Format with two decimal places
  openxlsx::addStyle(wb, sheet_name, two_decimal_style,
                     rows=data_rows, cols=data_cols,
                     gridExpand=TRUE, stack=TRUE)
}

#' Write an H-Score table to an Excel workbook
#'
#' Write a formatted H-Score table to a sheet in an Excel workbook.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param h_score A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and percent, such as the output of [compute_h_score].
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table. If omitted,
#' the title will be inferred from the `h_score` data if possible.
#' @family output functions
#' @export
write_h_score_sheet = function(wb, h_score,
                                sheet_name='H-Score',
                                sheet_title=NULL)
{
  measure = attr(h_score, 'measure') %>% remove_marker_mean
  if (is.null(sheet_title)) {
    sheet_title = ifelse(is.null(measure),
                         'H-Score', paste0('H-Score, ', measure))
  }

  d = h_score %>% dplyr::select(-Total) %>%
    dplyr::rename_at(3:6, ~stringr::str_remove(.x, 'Count of '))

  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header across all the data columns
  header_col = 3
  openxlsx::writeData(wb, sheet_name, startCol=header_col, sheet_title)
  openxlsx::addStyle(wb, sheet_name, bold_wrap_style, rows=1, cols=1:header_col)
  merge_and_outline_cells(wb, sheet_name, rows=1, cols=header_col:ncol(d))

  # Write two sub-heads
  openxlsx::writeData(wb, sheet_name, startCol=3, startRow=2, 'Cells per Bin')
  merge_and_outline_cells(wb, sheet_name, rows=2, cols=3:6)
  openxlsx::writeData(wb, sheet_name, startCol=7, startRow=2, 'Percent of Total Cells per Bin')
  merge_and_outline_cells(wb, sheet_name, rows=2, cols=7:10)
  openxlsx::addStyle(wb, sheet_name, bold_wrap_style, rows=2, cols=3:10, stack=TRUE)

  # Write the table
  openxlsx::writeData(wb, sheet_name, d, startRow=3,
                      headerStyle=bold_wrap_style, keepNA=TRUE)


  # Make the initial headers be multiple rows
  for (col in 1:2) {
    openxlsx::writeData(wb, sheet_name, startCol=col,
                        data.frame(xx=names(d)[col]), colNames=FALSE)
    merge_and_outline_cells(wb, sheet_name, rows=1:3, cols=col)
  }

  # The rest of the headers get outlined
  for (col in 3:(ncol(d)-1))
    outline_cells(wb, sheet_name, rows=3, cols=col)

  # H-Score column is special
  openxlsx::writeData(wb, sheet_name, startRow=2, startCol=11, 'H-Score')
  merge_and_outline_cells(wb, sheet_name, rows=2:3, cols=11)

  first_data_row = 4
  format_slide_id(wb, sheet_name, d, first_data_row)

  # Wider Tissue Category column
  openxlsx::setColWidths(wb, sheet_name, 2, 11)

  grid_spacing = add_grid_lines(wb, sheet_name, d, header_col, first_data_row)

  # Page formatting
  insert_page_breaks(wb, sheet_name, d, grid_spacing, num_title_rows=3)

  data_rows = 1:nrow(h_score)+2

  # Format as percent with one decimal place except for the last column
  # Showing one decimal makes the total add up
  openxlsx::addStyle(wb, sheet_name, percent_style.1,
                     rows=data_rows, cols=7:10,
                     gridExpand=TRUE, stack=TRUE)
}

#' Write a plot to an Excel workbook
#'
#' Write a plot to a sheet in an Excel workbook.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param plot A plot such as the output from [upset_plot].
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the plot.
#' @family output functions
#' @export
write_plot_sheet = function(wb, plot, sheet_name='Phenotypes',
                  sheet_title='All combinations of phenotypes in all slides')
{
  # Make a new sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header across all the data columns
  openxlsx::writeData(wb, sheet_name, sheet_title)
  openxlsx::addStyle(wb, sheet_name, bold_style,
                     rows=1, cols=1, stack=TRUE)

  print(plot)
  openxlsx::insertPlot(wb, sheet_name, startRow=3,
                       width=10, height=6, fileType='png')
}

#' Write a single sheet with formatting common to all sheets.
#'
#' - Bold, centered header
#' - Bold column headers
#' - Bold Slide ID column
#' - Two-row page title
#' - Page breaks as needed
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param d A data frame to write.
#' @param sheet_name Name for the worksheet.
#' @param sheet_title Title header for the plot.
#' @param header_col Column number to start the `sheet_title`
write_sheet <- function(wb, d, sheet_name, sheet_title, header_col) {
  # Make a new sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header across all the data columns
  openxlsx::writeData(wb, sheet_name, startCol=header_col, sheet_title)
  openxlsx::addStyle(wb, sheet_name, bold_wrap_style, rows=1, cols=1:header_col)
  merge_and_outline_cells(wb, sheet_name, rows=1, cols=header_col:ncol(d))

  # Write the table
  openxlsx::writeData(wb, sheet_name, d, startRow=2,
                      headerStyle=bold_wrap_style, keepNA=TRUE)

  first_data_row = 3

  # Make the initial headers be multiple rows
  for (col in 1:(header_col-1)) {
    openxlsx::writeData(wb, sheet_name, startCol=col,
                        data.frame(xx=names(d)[col]), colNames=FALSE)
    merge_and_outline_cells(wb, sheet_name, rows=1:2, cols=col)
  }

  # The rest of the headers get outlined
  for (col in header_col:ncol(d))
    outline_cells(wb, sheet_name, rows=2, cols=col)

  format_slide_id(wb, sheet_name, d, first_data_row)

  # Wider Tissue Category column
  openxlsx::setColWidths(wb, sheet_name, 2, 11)

  grid_spacing = add_grid_lines(wb, sheet_name, d, header_col, first_data_row)

  # Page formatting
  insert_page_breaks(wb, sheet_name, d, grid_spacing)
}

# Wider, bold Slide ID column
format_slide_id <- function(wb, sheet_name, d, first_data_row) {
  openxlsx::setColWidths(wb, sheet_name, 1, 'auto')
  openxlsx::addStyle(wb, sheet_name, bold_style,
                     rows=first_data_row:(nrow(d)+first_data_row-1), cols=1)
}

# Set up grid lines per slide if there are multiple tissue categories
add_grid_lines <- function(wb, sheet_name, d, header_col, first_data_row) {
  grid_spacing = length(unique(d$`Tissue Category`))
  if (grid_spacing > 1) {
    # Borders at left, right and bottom of the data portion
    last_data_row = nrow(d)+first_data_row-1
    openxlsx::addStyle(wb, sheet_name,
                       openxlsx::createStyle(border='Left'),
                       rows=first_data_row:last_data_row, cols=header_col,
                       stack=TRUE)
    openxlsx::addStyle(wb, sheet_name,
                       openxlsx::createStyle(border='Right'),
                       rows=first_data_row:last_data_row, cols=ncol(d),
                       stack=TRUE)
    openxlsx::addStyle(wb, sheet_name,
                       openxlsx::createStyle(border='Bottom'),
                       rows=last_data_row, 1:ncol(d),
                       stack=TRUE)

    # Top border at grid spacing
    for (start_row in seq(first_data_row, nrow(d)+first_data_row-grid_spacing, grid_spacing))
      openxlsx::addStyle(wb, sheet_name,
                         openxlsx::createStyle(border='Top'),
                         rows=start_row,
                         cols=1:ncol(d),
                         stack=TRUE)
  }

  grid_spacing
}

insert_page_breaks <- function(wb, sheet_name, d, grid_spacing,
                               num_title_rows = 2,
                               max_rows = 29 # Worst case is the expression worksheet
                               ) {
  # Print titles
  openxlsx::pageSetup(wb, sheet_name, orientation='landscape',
            printTitleRows = 1:num_title_rows)

  # Page breaks should fall on Slide ID boundaries
  if (nrow(d) + num_title_rows > max_rows) {
    rows_per_page =
      as.integer((max_rows - num_title_rows)/grid_spacing) * grid_spacing
    page_break = rows_per_page + num_title_rows
    while (page_break < nrow(d)) {
      openxlsx::pageBreak(wb, sheet_name, page_break)
      page_break = page_break + rows_per_page
    }
  }
}

merge_and_outline_cells = function(wb, sheet_name, rows, cols) {
  openxlsx::mergeCells(wb, sheet_name, rows=rows, cols=cols)
  outline_cells(wb, sheet_name, rows, cols)
}

# Outline the block of cells given by rows x cols
outline_cells = function(wb, sheet_name, rows, cols) {
  openxlsx::addStyle(wb, sheet_name,
                     openxlsx::createStyle(border='Top'),
                     rows=rows[1],
                     cols=cols,
                     stack=TRUE)

  openxlsx::addStyle(wb, sheet_name,
                     openxlsx::createStyle(border='Left'),
                     rows=rows,
                     cols=cols[1],
                     stack=TRUE)

  openxlsx::addStyle(wb, sheet_name,
                     openxlsx::createStyle(border='Bottom'),
                     rows=utils::tail(rows, 1),
                     cols=cols,
                     stack=TRUE)

  openxlsx::addStyle(wb, sheet_name,
                     openxlsx::createStyle(border='Right'),
                     rows=rows,
                     cols=utils::tail(cols, 1),
                     stack=TRUE)
}

# Remove " (xx xx) Mean" from strings
remove_marker_mean = function(s) {
  stringr::str_remove(s, ' \\(.*\\) Mean$')
}
