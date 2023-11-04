# Excel helpers
# Use openxlsx to write formatted worksheets with analysis results.

# Cell styles
percent_style = openxlsx::createStyle(numFmt='0%')
percent_style_1 = openxlsx::createStyle(numFmt='0.0%')
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
                               sheet_name='Slide Summary') {
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
                              sheet_title='Cell Counts per Phenotype') {
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
                              sheet_title='Percentage of Total Cells') {
  added_columns = write_sheet(wb, percents, sheet_name, sheet_title, 3)

  data_rows = 1:nrow(percents)+2
  data_cols = 3:ncol(percents) + added_columns

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
                               sheet_title='Cell Densities (cells/mm2)') {
  added_columns = write_sheet(wb, densities, sheet_name, sheet_title, 4)

  # Border to the left of the Area column
  openxlsx::addStyle(wb, sheet_name,
                     openxlsx::createStyle(border='Left'),
                     rows=3:(nrow(densities)+2), cols=3+added_columns,
                     stack=TRUE)

  # Format tissue area with two decimal places, densities as integer
  data_rows = 1:nrow(densities)+2
  openxlsx::addStyle(wb, sheet_name, two_decimal_style,
                     rows=data_rows, cols=3+added_columns, stack=TRUE)
  openxlsx::addStyle(wb, sheet_name, integer_style, rows=data_rows,
                     cols=4:ncol(densities) + added_columns,
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
    dplyr::select(1, `Tissue Category`, dplyr::everything()) %>%
    dplyr::select(-dplyr::contains('Count')) %>%
    dplyr::rename_all(remove_marker_mean)

  added_columns = write_sheet(wb, exprs, sheet_name, sheet_title, 3)

  # Make the expression columns a little wider
  data_rows = 1:nrow(exprs)+2
  data_cols = 3:ncol(exprs) + added_columns
  openxlsx::setColWidths(wb, sheet_name, data_cols, 14)

  # Format with two decimal places
  openxlsx::addStyle(wb, sheet_name, two_decimal_style,
                     rows=data_rows, cols=data_cols,
                     gridExpand=TRUE, stack=TRUE)
}

#' Write a nearest neighbor summary to an Excel workbook
#'
#' Write a formatted nearest neighbor summary table to a
#' sheet in an Excel workbook.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param stats A summary data frame.
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @family output functions
#' @export
write_nearest_neighbor_summary_sheet = function(wb, stats,
      sheet_name='Nearest Neighbors',
      sheet_title='Nearest Neighbor Distances for Phenotype Pairs (microns)') {
  added_columns = write_sheet(wb, stats, sheet_name, sheet_title, 2)

  data_rows = 1:nrow(stats)+2
  data_cols = 4:ncol(stats) + added_columns

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
#' @param marker Optional marker name to add to the default sheet title.
#' Ignored if `sheet_title` is provided.
#' @family output functions
#' @export
write_h_score_sheet = function(wb, h_score,
                               sheet_name=NULL,
                               sheet_title=NULL,
                               marker=NULL) {
  measure = attr(h_score, 'measure') %>% remove_marker_mean
  if (is.null(sheet_title)) {
    sheet_title = ifelse(is.null(measure),
                         'H-Score', paste0('H-Score, ', measure))
    if (!is.null(marker))
      sheet_title = paste0(sheet_title, ', ', remove_marker_mean(marker))
  }

  # Sheet name is the sheet title without the compartment name
  if (is.null(sheet_name))
    sheet_name = sheet_title %>%
      stringr::str_remove_all('(Nucleus|Cytoplasm|Membrane|Entire Cell) ')
  sheet_name = as_valid_tab_name(sheet_name)

  d = h_score %>%
    dplyr::select(-Total)

  # These names duplicate columns 7-10
  # In the worksheet, they will have subheads to disambiguate them
  names(d)[3:6] = c("0+", "1+", "2+", "3+")

  # Add TMA columns, if any
  added_columns = 0
  tma_cols = parse_tma_columns(d)
  if (!is.null(tma_cols)) {
    added_columns = ncol(tma_cols)
    d = tibble::add_column(d, !!!tma_cols, .after=1, .name_repair='minimal')
  }

  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header across all the data columns
  header_col = 3 + added_columns
  openxlsx::writeData(wb, sheet_name, startCol=header_col, sheet_title)
  openxlsx::addStyle(wb, sheet_name, bold_wrap_style, rows=1, cols=1:header_col)
  merge_and_outline_cells(wb, sheet_name, rows=1, cols=header_col:ncol(d))

  # Write two sub-heads
  openxlsx::writeData(wb, sheet_name, startCol=3 + added_columns, startRow=2,
                      'Cells per Bin')
  merge_and_outline_cells(wb, sheet_name, rows=2, cols=3:6 + added_columns)
  openxlsx::writeData(wb, sheet_name,
                      startCol=7 + added_columns, startRow=2,
                      'Percent of Total Cells per Bin')
  merge_and_outline_cells(wb, sheet_name, rows=2, cols=7:10 + added_columns)
  openxlsx::addStyle(wb, sheet_name, bold_wrap_style,
                     rows=2, cols=3:10 + added_columns, stack=TRUE)

  # Write the table
  openxlsx::writeData(wb, sheet_name, d, startRow=3,
                      headerStyle=bold_wrap_style, keepNA=TRUE)


  # Make the initial headers be multiple rows
  for (col in 1:(2 + added_columns)) {
    openxlsx::writeData(wb, sheet_name, startCol=col,
                        data.frame(xx=names(d)[col]), colNames=FALSE)
    merge_and_outline_cells(wb, sheet_name, rows=1:3, cols=col)
  }

  # The rest of the headers get outlined
  for (col in (3 + added_columns):(ncol(d)-1))
    outline_cells(wb, sheet_name, rows=3, cols=col)

  # H-Score column is special
  openxlsx::writeData(wb, sheet_name, startRow=2, startCol=11 + added_columns,
                      'H-Score')
  merge_and_outline_cells(wb, sheet_name, rows=2:3, cols=11 + added_columns)

  first_data_row = 4
  format_slide_id(wb, sheet_name, d, first_data_row)

  # Narrower TMA columns
  if (added_columns > 0)
    openxlsx::setColWidths(wb, sheet_name, 1:added_columns + 1, 7)

  # Wider Tissue Category column
  openxlsx::setColWidths(wb, sheet_name, 2 + added_columns, 11)

  grid_spacing = add_grid_lines(wb, sheet_name, d, header_col, first_data_row)

  # Page formatting
  insert_page_breaks(wb, sheet_name, d, grid_spacing, num_title_rows=3)

  data_rows = 1:nrow(h_score)+2

  # Format as percent with one decimal place except for the last column
  # Showing one decimal makes the total add up
  openxlsx::addStyle(wb, sheet_name, percent_style_1,
                     rows=data_rows, cols=7:10 + added_columns,
                     gridExpand=TRUE, stack=TRUE)
}

#' Write a "count within" summary to an Excel workbook
#'
#' Write a formatted "count within" summary table to a
#' sheet in an Excel workbook.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param stats A summary data frame.
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @family output functions
#' @export
write_count_within_sheet = function(wb, stats, sheet_name='Count Within',
                   sheet_title='Count of cells within the specified radius') {
  write_sheet(wb, stats, sheet_name, sheet_title, 3, addGrid=FALSE)
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
                  sheet_title='All combinations of phenotypes in all slides') {
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
#'
#' If the provided data has TMA core information embedded in a
#' `Sample Name` column, add columns with the TMA info.
#'
#' @param wb An openxlsx Workbook from [openxlsx::createWorkbook]
#' @param d A data frame to write.
#' @param sheet_name Name for the worksheet.
#' @param sheet_title Title header for the plot.
#' @param header_col Column number to start the `sheet_title`
#' @param keepNA If TRUE, NA values are written as #N/A; else they are blank.
#' @param addGrid If TRUE, grid lines (and page breaks) are added based
#' on the tissue categories in the data. If FALSE, no grid lines are added
#' and page breaks are added where needed.
#' @return Invisibly, the number of columns added with TMA info (0, 3, or 4)
#' @export
write_sheet <- function(wb, d, sheet_name, sheet_title, header_col,
                        keepNA=TRUE, addGrid=TRUE) {
  added_columns = 0
  tma_cols = parse_tma_columns(d)
  if (!is.null(tma_cols)) {
    added_columns = ncol(tma_cols)
    d = tibble::add_column(d, !!!tma_cols, .after=1)
    header_col = header_col + added_columns
  }
  # Make a new sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header across all the data columns
  # writeData doesn't like 'glue' objects so unclass.
  openxlsx::writeData(wb, sheet_name, startCol=header_col, unclass(sheet_title))
  openxlsx::addStyle(wb, sheet_name, bold_wrap_style, rows=1, cols=1:header_col)
  merge_and_outline_cells(wb, sheet_name, rows=1, cols=header_col:ncol(d))

  # Write the table
  openxlsx::writeData(wb, sheet_name, d, startRow=2,
                      headerStyle=bold_wrap_style, keepNA=keepNA)

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

  # Narrower TMA columns
  if (added_columns > 0)
    openxlsx::setColWidths(wb, sheet_name, 1:added_columns + 1, 7)

  # Wider Tissue Category column
  tissue_col = match('Tissue Category', names(d))
  if (!is.na(tissue_col))
    openxlsx::setColWidths(wb, sheet_name, tissue_col, 11)

  if (addGrid) {
    grid_spacing = add_grid_lines(wb, sheet_name, d, header_col, first_data_row)
  } else {
    grid_spacing = 1
  }

  # Page formatting
  insert_page_breaks(wb, sheet_name, d, grid_spacing)

  invisible(added_columns)
}

# Parse TMA columns from Sample Name
# @param d A tibble, possibly containing a Sample Name column
# @return If `d` contains Sample Names with Core IDs, a tibble with
# columns TMA Sector, TMA Row, TMA Column, and possibly TMA Field.
# If `d` does not contain TMA information, return `NULL`.
# Note: the logic here is based on `GetTMAInfoFromName` in MangoLib.
parse_tma_columns = function(d) {
  if (!'Sample Name' %in% names(d)) return(NULL)

  re1 = stringr::regex("core\\[([,\\w]+)\\]_", ignore_case=TRUE)

  # Check the first item for a match so we can fail fast
  if (!stringr::str_detect(d$`Sample Name`[1], re1))
    return(NULL)

  name_candidates = c('TMA Sector', 'TMA Row', 'TMA Column', 'TMA Field')
  tma_cols = d$`Sample Name` %>%
    stringr::str_match(re1) %>%
    `[`(,2, drop=TRUE) %>% # Get just the id portion
    stringr::str_split(',') %>%
    purrr::transpose() %>% # Convert to column lists
    purrr::simplify_all() %>%
    tibble::as_tibble(.name_repair='minimal') %>%
    rlang::set_names(name_candidates[1:ncol(.)])

  # Convert to numeric if possible
  numeric_cols = !is.na(suppressWarnings(as.numeric(tma_cols[1,])))
  tma_cols = tma_cols %>%
    dplyr::mutate(dplyr::across(which(numeric_cols), as.numeric))

  tma_cols
}

# Wider, bold Slide ID column
format_slide_id <- function(wb, sheet_name, d, first_data_row) {
  openxlsx::setColWidths(wb, sheet_name, 1, 'auto')
  openxlsx::addStyle(wb, sheet_name, bold_style,
                     rows=first_data_row:(nrow(d)+first_data_row-1), cols=1)
}

# Set up grid lines per slide if there are multiple tissue categories
add_grid_lines <- function(wb, sheet_name, d, header_col, first_data_row) {
  if (!'Tissue Category' %in% names(d)) return(1)

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
                       rows=last_data_row, seq_along(d),
                       stack=TRUE)

    # Top border at grid spacing
    border_rows = seq(first_data_row,
                      nrow(d)+first_data_row-grid_spacing,
                      grid_spacing)
    openxlsx::addStyle(wb, sheet_name,
                         openxlsx::createStyle(border='Top'),
                         rows=border_rows,
                         cols=seq_along(d),
                         gridExpand=TRUE,
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

# Merge cells in a worksheet and outline the result
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

# Remove " (xx xx) Mean" from strings, cast symbols to strings before use
remove_marker_mean = function(s) {
  stringr::str_remove_all(as.character(s), ' \\(.*?\\) Mean')
}

# Create valid Excel worksheet tab names from the given strings
# Tab names can't be more than 31 characters and may not include any of \/*?:[]
as_valid_tab_name = function(strs) {
  strs %>%
    stringr::str_replace_all('[\\\\/*?:\\[\\]]', '_') %>%
    substr(1, 31)
}
