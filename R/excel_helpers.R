# Excel helpers

percent_style = openxlsx::createStyle(numFmt='0%')
two_decimal_style = openxlsx::createStyle(numFmt='0.00')
integer_style = openxlsx::createStyle(numFmt='0')
bold_style = openxlsx::createStyle(textDecoration='bold',
                                   halign='center', wrapText=TRUE)

#' Write a cell counts table to an Excel workbook
#'
#' @param wb An openxlsx Workbook
#' @param counts A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and counts, such as the output of [count_phenotypes()].
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @export
write_counts_sheet = function(wb, counts,
                              sheet_name='Cell Counts',
                              sheet_title='Cell Counts per Phenotype')
{
  write_sheet(wb, counts, sheet_name, sheet_title, 3)
}

#' Write a cell percent table to an Excel workbook
#'
#' @param wb An openxlsx Workbook
#' @param percents A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and percent.
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @export
write_percents_sheet = function(wb, percents,
                              sheet_name='Cell Percents',
                              sheet_title='Percentage of Total Cells')
{
  write_sheet(wb, percents, sheet_name, sheet_title, 3)

  data_rows = 1:nrow(percents)+2
  data_cols = 3:ncol(percents)

  # Format as percent
  openxlsx::addStyle(wb, sheet_name, percent_style,
                     rows=data_rows, cols=data_cols, gridExpand=TRUE)
}

#' Write a density table to an Excel workbook
#'
#' @param wb An openxlsx Workbook
#' @param densities A data frame with columns for `Slide ID`, `Tissue Category`,
#'   `Tissue Area` and densities, such as the output of [compute_density()].
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @export
write_density_sheet = function(wb, densities,
                               sheet_name='Cell Densities',
                               sheet_title='Cell Densities (cells/mm2)')
{
  write_sheet(wb, densities, sheet_name, sheet_title, 4)

  # Format tissue area with two decimal places, densities as integer
  data_rows = 1:nrow(densities)+2
  openxlsx::addStyle(wb, sheet_name, two_decimal_style, rows=data_rows, cols=3)
  openxlsx::addStyle(wb, sheet_name, integer_style, rows=data_rows,
                     cols=4:ncol(densities), gridExpand=TRUE)
}

#' Write an expression table to an Excel workbook
#'
#' @param wb An openxlsx Workbook
#' @param exprs A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and mean expression, such as the output of
#'   [compute_mean_expression_many()]. Count columns will not be reported.
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @importFrom magrittr %>%
#' @export
write_expression_sheet = function(wb, exprs,
                               sheet_name='Mean Expression',
                               sheet_title='Mean Expression') {
  # Subset and re-order columns
  exprs = exprs %>%
    dplyr::select(`Slide ID`, `Tissue Category`, dplyr::everything()) %>%
    dplyr::select(-dplyr::contains('Count'))

  write_sheet(wb, exprs, sheet_name, sheet_title, 3)

  # Make the expression columns a little wider
  data_rows = 1:nrow(exprs)+2
  data_cols = 3:ncol(exprs)
  openxlsx::setColWidths(wb, sheet_name, data_cols, 14)

  # Format with two decimal places
  openxlsx::addStyle(wb, sheet_name, two_decimal_style,
                     rows=data_rows, cols=data_cols, gridExpand=TRUE)
}

#' Write an H-Score table to an Excel workbook
#'
#' @param wb An openxlsx Workbook
#' @param h_score A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and percent.
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @export
write_h_score_sheet = function(wb, h_score,
                                sheet_name='H-Score',
                                sheet_title='H-Score')
{
  write_sheet(wb, h_score, sheet_name, sheet_title, 3)

  data_rows = 1:nrow(h_score)+2

  # Format as percent
  openxlsx::addStyle(wb, sheet_name, percent_style,
                     rows=data_rows, cols=8:11, gridExpand=TRUE)
}

write_sheet <- function(wb, d, sheet_name, sheet_title, header_col) {
  # Make a new sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header
  openxlsx::writeData(wb, sheet_name, startCol=header_col, sheet_title)
  openxlsx::addStyle(wb, sheet_name, bold_style, rows=1, cols=1:header_col)
  openxlsx::mergeCells(wb, sheet_name, rows=1, cols=header_col:ncol(d))

  # Write the table
  openxlsx::writeData(wb, sheet_name, d, startRow=2, headerStyle=bold_style)

  # Make the initial headers be two rows
  for (col in 1:(header_col-1)) {
    openxlsx::writeData(wb, sheet_name, startCol=col,
                        data.frame(xx=names(d)[col]), colNames=FALSE)
    openxlsx::mergeCells(wb, sheet_name, rows=1:2, cols=col)
  }

  # Wider first column
  openxlsx::setColWidths(wb, sheet_name, 1, 'auto')
}

