# Top 20 and bottom 10%ile

#' Create reports showing mean expression of top and bottom cells
#' @param merge_file Path to a merged or consolidated cell seg data file
#' @param config_file Path to a configuration file containing
#' column names in the merge file, one name per line.
#' @param by Column to aggregate by, e.g. "Slide ID" or "Annotation ID".
#' @param tissue_categories Tissue categories to report, or NULL to use all.
#' @param adjacent_max The maximum ratio between expression of adjacent fluors.
#' @return None
#' @export
mean_of_top_and_bottom_cells_report =
  function(merge_file, config_file, by, tissue_categories, adjacent_max) {
    # Read the configuration. Each line should be a column name
    cols = readLines(config_file) %>%
      purrr::map_chr(stringr::str_trim) %>%
      purrr::discard(~.x=="")

    # Create the Excel file
    excel_path = mean_of_top_and_bottom_cells(
      merge_file, cols, adjacent_max=adjacent_max,
      tissue_categories=tissue_categories,
      .by=by)

    # Create the charts
    charts_path = stringr::str_replace(excel_path, 'xlsx$', 'docx')
    write_mean_of_top_and_bottom_charts(excel_path, charts_path, .by=by)

    cat('Reports written to\n', excel_path, '\n', charts_path, '\n')
}

#' Compute mean expression levels of the top and bottom expressing cells,
#' per Slide ID or field. This creates a quality report.
#'
#' Report the results in an Excel workbook in the same directory
#'   as `csd_path`.
#' @param csd_path Path to a merged cell seg data, or NULL. If NULL, a file
#'   chooser is opened to allow file selection.
#' @param expression_cols Vector of column names to report.
#' @param top_count The number of top-expressing cells to select for
#'   averaging.
#' @param bottom_percentile The cutoff for the bottom percentile cells.
#' @param adjacent_max The maximum ratio between expression of adjacent fluors
#' @param tissue_categories Tissue categories to include, or NULL for all
#' @param .by The column to aggregate by
#' @param out_path The path to the output file; if `NULL`, a date-stamped file
#' will be written to the same directory as `csd_path`.
#' @return The path to the generated file.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
mean_of_top_and_bottom_cells =
  function(csd_path=NULL, expression_cols,
           top_count=20, bottom_percentile=0.1, adjacent_max = 3,
           tissue_categories=NULL, .by='Slide ID', out_path=NULL)
{
  if (is.null(csd_path))
    csd_path = file.choose()

  # Read the merged cell seg data file
  csd = phenoptr::read_cell_seg_data(csd_path)

  if (!is.null(tissue_categories) && length(tissue_categories) > 0)
    csd = csd %>% dplyr::filter(.data$`Tissue Category` %in% tissue_categories)

  # Define phenotypes and expression
  phenotypes = phenoptr::parse_phenotypes('Total Cells')

  # Check that all the expression columns exist
  missing = !(expression_cols %in% names(csd))
  if (any(missing)) {
    missing = expression_cols[missing]
    stop('Columns missing from cell seg data: ', paste(missing, collapse=', '))
  }


  params = rlang::set_names(expression_cols,
                            rep('Total Cells', length(expression_cols))) %>%
    as.list()

  # Compute top and bottom means
  top20 = phenoptrReports::compute_mean_expression_many(
    csd, phenotypes, params, count=top_count, .by=.by) %>%
    dplyr::select(!!.by, dplyr::everything())
  bottom10 = phenoptrReports::compute_mean_expression_many(
    csd, phenotypes, params,  percentile=-bottom_percentile, .by=.by) %>%
    dplyr::select(!!.by, dplyr::everything())

  # Clean up column names, remove everything before the compartment and ' Mean'
  clean_names = function(n) {
    n %>% stringr::str_remove('^.*(?=Nucleus|Cytoplasm|Membrane)') %>%
      stringr::str_remove(' Mean')
  }

  names(top20) = clean_names(names(top20))
  names(bottom10) = clean_names(names(bottom10))

  # Ratio of means, top / bottom
  ratio_top_to_bottom = purrr::map2(top20, bottom10, function(t, b) {
    if (!is.numeric(t)) return(t)
    t / b
  }) %>% tibble::as_tibble()

  # Ratio of means of top cells, adjacent fluors
  ratio_data = top20 %>%
    dplyr::select(-!!.by)
  first_fluors = ratio_data %>% dplyr::select(-ncol(ratio_data))
  second_fluors = ratio_data %>% dplyr::select(-1)
  ratio_adjacent = purrr::map2(first_fluors, second_fluors, ~.x/.y) %>%
    purrr::set_names(purrr::map2(names(first_fluors), names(second_fluors),
                                 ~paste0(.x, ' / ', .y))) %>%
    tibble::as_tibble() %>%
    tibble::add_column(top20[.by], .before=1)

  # Write it out
  red_style = openxlsx::createStyle(fontColour='red')

  # How many non-data columns are there?
  # The main header starts in the next column
  header_col = 2

  if (is.null(out_path))
    out_path = file.path(dirname(csd_path),
      stringr::str_glue(
        "{format(Sys.Date(), '%Y%m%d')}_Top{top_count}_Bottom{bottom_percentile*100}_Data.xlsx"))
  wb = openxlsx::createWorkbook()

  write_and_style_sheet(wb, top20,
            stringr::str_glue('Top {top_count} data'),
            stringr::str_glue('Mean expression of top {top_count} cells'),
            header_col)

  write_and_style_sheet(wb, bottom10,
    stringr::str_glue('Bottom {bottom_percentile*100}%ile data'),
    stringr::str_glue(
      'Mean expression of bottom {bottom_percentile*100}%ile cells'),
    header_col)

  sheet_name = 'Ratio top to bottom'
  write_and_style_sheet(wb, ratio_top_to_bottom,
    sheet_name,
    stringr::str_glue(
      'Ratio of means, top {top_count} cells / bottom {bottom_percentile*100}%ile cells'),
    header_col)

  # Mark out-of-range rows for Opal 780 ratio if present
  col_780 = stringr::str_subset(names(ratio_top_to_bottom), '780')
  if (length(col_780)==1) {
    top_to_bottom_min = 30 # Opal 780 ratio should be greater than this

    red_rows = which(ratio_top_to_bottom[[col_780]] < top_to_bottom_min)
    if (length(red_rows) > 0) {
      red_rows = red_rows + 2 # skip header rows
      red_cols = which(names(ratio_top_to_bottom) == col_780)
      openxlsx::addStyle(wb, sheet_name, red_style,
                         rows=red_rows, cols=red_cols,
                         gridExpand=TRUE, stack=TRUE)
    }
  }

  sheet_name = 'Ratio adjacent fluors'
  write_and_style_sheet(wb, ratio_adjacent,
     sheet_name,
     stringr::str_glue(
       'Ratio of means, top {top_count} cells of adjacent fluors'),
     header_col)

  red_cells = (ratio_adjacent[-1] > adjacent_max
               | ratio_adjacent[-1] < 1/adjacent_max)
  red_cells = which(as.matrix(red_cells), arr.ind=TRUE)
  if (dim(red_cells)[1] > 0) {
    openxlsx::addStyle(wb, sheet_name, red_style,
                       rows=red_cells[,'row']+2, cols=red_cells[,'col']+1,
                       gridExpand=FALSE, stack=TRUE)
  }

  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

  return(out_path)
}

write_and_style_sheet =
    function(wb, d, sheet_name, sheet_title, header_col) {
  phenoptrReports::write_sheet(wb, d, sheet_name, sheet_title, header_col)
  data_cols = header_col:ncol(d)
  openxlsx::setColWidths(wb, sheet_name, data_cols, 14)

  number_style=openxlsx::createStyle(numFmt='0.0000')
  openxlsx::addStyle(wb, sheet_name, number_style,
                     rows=seq_len(nrow(d))+2, cols=data_cols,
                     gridExpand=TRUE, stack=TRUE)
}

#' Create summary charts from the results of `mean_of_top_and_bottom_cells`
#'
#' Create a Microsoft Word file or HTML document containing summary charts
#' derived from the output of [mean_of_top_and_bottom_cells]
#' with default parameters.
#' The file type is determined by the file extension
#' of `output_path`, which must be either `.docx` or `.html`.
#' @param worksheet_path Path to an Excel file containing sheets written
#'   by [mean_of_top_and_bottom_cells].
#' @param output_path Path to write the resulting file.
#' @param .by The aggregation column name
#' @export
write_mean_of_top_and_bottom_charts =
    function(worksheet_path, output_path, .by='Slide ID') {
  stopifnot(file.exists(worksheet_path))

  if (is.null(output_path))
    stop('You must provide an output path.')

  output_format = switch(tools::file_ext(output_path),
                         docx='word_document',
                         html='html_vignette')

  if (is.null(output_format))
    stop('Unsupported output format')

  rmd_path = system.file("rmd", "Mean_of_top_and_bottom_charts.Rmd",
                         package="phenoptrReports")

  rmarkdown::render(rmd_path, output_file=output_path, quiet=TRUE,
                    output_format=output_format,
                    params=list(data_path=worksheet_path, .by=.by))
}
