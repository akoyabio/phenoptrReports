# Excel helpers

pct_style = openxlsx::createStyle(numFmt='0%')
bold_style = openxlsx::createStyle(textDecoration='bold',
                                   halign='center')

#' Write a counts table to an Excel workbook
#'
#' @param wb An openxlsx Workbook
#' @param counts A data frame with columns for `Slide ID`, `Tissue Category`,
#'   and counts, such as the output of [count_phenotypes()].
#' @param sheet_name Optional name for the worksheet.
#' @param sheet_title Optional title header for the table.
#' @export
write_counts_sheet = function(wb, counts,
                              sheet_name='Cell Counts',
                              sheet_title='Cell Counts')
{
  write_sheet(wb, counts, sheet_name, sheet_title, 3)
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
}

write_sheet <- function(wb, d, sheet_name, sheet_title, header_col) {
  # Make a new sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header
  openxlsx::writeData(wb, sheet_name, startCol=header_col,
                      data.frame(title=sheet_title), colNames=FALSE)
  openxlsx::addStyle(wb, sheet_name, bold_style, rows=1, cols=header_col)
  openxlsx::mergeCells(wb, sheet_name, rows=1, cols=header_col:ncol(d))

  # Write the table
  openxlsx::writeData(wb, sheet_name, d, startRow=2, headerStyle=bold_style)
}

