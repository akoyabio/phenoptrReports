library(testthat)
library(dplyr)
library(tidyr)
library(readxl)

test_file_generation = function(data_dir, output_dir, expected_path, .by) {
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive=TRUE)
    Sys.sleep(0.1) # Wait for it...
  }
  dir.create(output_dir)

  # Data structure for format_all
  all_data = list(
    by=.by,
    use_regex = FALSE,
    slide_id_prefix = "Set",
    tissue_categories = c("Tumor", "Stroma"),
    input_path = file.path(data_dir, "Consolidated_data.txt"),
    summary_path = file.path(data_dir, "Merge_cell_seg_data_summary.txt"),
    score_path = file.path(data_dir, "Merge_score_data.txt"),
    output_dir = output_dir,
    field_col = 'Sample Name',
    include_nearest = TRUE,
    include_count_within = TRUE,
    include_distance_details = TRUE,
    radii = c(10, 15),
    phenotype_values = list(
      list(phenotype = "CD8+", expression = "Membrane PDL1 (Opal 520) Mean"),
      list(phenotype = "CD8+", expression = "Membrane PD1 (Opal 650) Mean", score=TRUE),
      list(phenotype = "CD68+", expression = "Membrane PDL1 (Opal 520) Mean"),
      list(phenotype = "FoxP3+", expression = 'NA'),
      list(phenotype = "CK+", expression = "Membrane PDL1 (Opal 520) Mean"),
      list(phenotype = "CK+/~`Membrane PDL1 (Opal 520) Mean`>1", expression='NA', score=TRUE),
      list(phenotype = "Total Cells", expression = 'NA')
    ))

  # Get the formatter function from the app
  source(system.file('analysis_app', 'formatters.R', package='phenoptrReports'),
         local=TRUE)
  script = format_all(all_data)
  script_path = file.path(output_dir, 'Script.R')
  readr::write_lines(script, script_path)

  # Run the script to do some work!
  source(script_path, local=new.env())

  # Check that we created the correct files
  actual_path = file.path(output_dir, 'Results.xlsx')

  expect_true(file.exists(file.path(output_dir, 'Script.R')))
  expect_true(file.exists(actual_path))
  expect_true(file.exists(file.path(output_dir, 'Charts.docx')))
  expect_true(file.exists(file.path(output_dir, 'nearest_neighbors_Tumor.txt')))
  expect_true(file.exists(file.path(output_dir, 'nearest_neighbors_Stroma.txt')))
  expect_true(file.exists(file.path(output_dir, 'nearest_neighbors_All.txt')))
  expect_true(file.exists(file.path(output_dir, 'count_within_Tumor.txt')))
  expect_true(file.exists(file.path(output_dir, 'count_within_Stroma.txt')))
  expect_true(file.exists(file.path(output_dir, 'count_within_All.txt')))
  expect_true(file.exists(file.path(output_dir, 'session_info.txt')))

  # Check numbers against known good

  sheets = excel_sheets(expected_path)
  expect_equal(excel_sheets(actual_path), sheets)

  for (sheet in sheets) {
    skip = ifelse(startsWith(sheet, 'H-Score'), 2, 1)
    actual_sheet =
      read_excel(actual_path, sheet, skip=skip, .name_repair='minimal')
    expected_sheet =
      read_excel(expected_path, sheet, skip=skip, .name_repair='minimal')
    expect_equal(actual_sheet, expected_sheet, info=paste('Sheet name:', sheet))
  }

  # Additional sanity checks on aggregation
  # (on both actual and expected, since they are the same)
  check_nearest_neighbors(actual_path)
  check_count_within(actual_path)
}

check_nearest_neighbors = function(actual_path) {
  df = read_excel(actual_path, 'Nearest Neighbors',
                  skip=1, .name_repair='minimal') %>%
    select(-Median, -SD) %>%
    pivot_wider(names_from='Tissue Category', values_from=Min:Max)

  # Because adding more candidate cells can only decrease the nearest neighbor
  # distance, these should all be true:
  expect_true(all(df$Min_All<=pmin(df$Min_Stroma, df$Min_Tumor), na.rm=TRUE))

  # OK almost true. Field 4_1-6plex_[15206,60541].im3 has no CD68+ in Tumor
  # and the nn distance from FoxP3+ to CD68+ in All happens to be the max,
  # so in the by Slide ID aggregation there is one row where this is not true.
  expect_lte(sum(df$Max_All>pmax(df$Max_Stroma, df$Max_Tumor), na.rm=TRUE), 1)

  expect_true(all(df$Mean_All<=pmax(df$Mean_Stroma, df$Mean_Tumor), na.rm=TRUE))
}

check_count_within = function(actual_path) {
  df = read_excel(actual_path, 'Count Within',
                  skip=1, .name_repair='minimal') %>%
    select(-`Within mean`) %>%
    pivot_wider(names_from='Tissue Category',
                values_from=`From count`:`From with`)

  # From and To counts should directly add
  expect_true(
    all(df$`From count_All`==df$`From count_Stroma`+df$`From count_Tumor`))
  expect_true(
    all(df$`To count_All`==df$`To count_Stroma`+df$`To count_Tumor`))

  # `From with` should be at least as big as the sum of the components
  expect_true(
    all(df$`From with_All`>=df$`From with_Stroma`+df$`From with_Tumor`))
}

# Normalize the path to a directory, creating it if needed
# On macOS, normalizePath doesn't create an absolute path if the
# target doesn't exist; this is a workaround
normalize_dir = function(rel_path) {
  if (!dir.exists(rel_path)) dir.create(rel_path)
  normalizePath(rel_path, winslash='/', mustWork=FALSE)
}

test_that("file generation works", {
  output_dir = normalize_dir(test_path('results'))

  data_dir = normalize_dir(test_path('test_data'))
  expected_path = file.path(data_dir, 'Results.xlsx')

  # Aggregate by Slide ID
  test_file_generation(data_dir, output_dir, expected_path, .by='Slide ID')

  # And by Sample Name
  output_dir = normalize_dir(test_path('results_by_sample'))
  expected_path = file.path(data_dir, 'Results_by_sample.xlsx')
  test_file_generation(data_dir, output_dir, expected_path, .by='Sample Name')
})

# Test writing charts with multiple pages per chart
test_that("Chart segmentation works", {
  workbook_path = normalizePath(test_path('test_data/Results_by_sample.xlsx'),
                                winslash='/', mustWork=FALSE)
  charts_path = normalize_dir(test_path('results_by_sample'))
  charts_path = file.path(charts_path, 'Charts_segmented.docx')

  if (!dir.exists(dirname(charts_path))) dir.create(dirname(charts_path))
  if (file.exists(charts_path)) file.remove(charts_path)
  write_summary_charts(workbook_path, charts_path, .by='Sample Name',
                       max_slides_per_plot=5)
  expect(file.exists(charts_path), 'Failed to create segmented charts document')
})

# Some really basic smoke tests - just make sure the reports run
# These work on dev machine only for now :-/

test_that("Unmixing quality report runs", {
  export_path = 'C:/Research/phenoptrTestData/unmixing_quality_report_test'
  skip_if_not(dir.exists(export_path))

  report_path = file.path(export_path, 'Unmixing_quality_report.html')
  if (file.exists(report_path)) file.remove(report_path)

  unmixing_quality_report(export_path)
  expect(file.exists(report_path), 'Failed to create unmixing quality report')
})

test_that("Component levels report runs", {
  export_path = 'C:/Research/phenoptrTestData/component_levels_report_test'
  skip_if_not(dir.exists(export_path))

  report_path = file.path(export_path, 'Component_levels_report.html')
  if (file.exists(report_path)) file.remove(report_path)

  component_levels_report(export_path)
  expect(file.exists(report_path), 'Failed to create component levels report')
})
