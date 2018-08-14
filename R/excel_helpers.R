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
                              sheet_title='Cell Counts') {
  # Make a new sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header
  openxlsx::writeData(wb, sheet_name, startCol=3,
            data.frame(title=sheet_title), colNames=FALSE)
  openxlsx::addStyle(wb, sheet_name, bold_style, rows=1, cols=3)
  openxlsx::mergeCells(wb, sheet_name, rows=1, cols=3:ncol(counts))

  # Write the table
  openxlsx::writeData(wb, sheet_name, counts, startRow=2, headerStyle=bold_style)
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
                              sheet_title='Cell Densities (cells/mm2)') {
  # Make a new sheet
  openxlsx::addWorksheet(wb, sheet_name)

  # Write a bold header
  openxlsx::writeData(wb, sheet_name, startCol=4,
                      data.frame(title=sheet_title), colNames=FALSE)
  openxlsx::addStyle(wb, sheet_name, bold_style, rows=1, cols=4)
  openxlsx::mergeCells(wb, sheet_name, rows=1, cols=4:ncol(densities))

  # Write the table
  openxlsx::writeData(wb, sheet_name, densities, startRow=2, headerStyle=bold_style)
}
