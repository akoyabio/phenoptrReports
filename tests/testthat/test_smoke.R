context("smoke")
library(testthat)
library(readxl)

test_that("file generation works", {
  # Make a test directory
  data_dir = normalizePath(test_path('test_data'), winslash='/', mustWork=FALSE)
  output_dir = normalizePath(test_path('results'), winslash='/', mustWork=FALSE)
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive=TRUE)
    Sys.sleep(0.1) # Wait for it...
  }
  dir.create(output_dir)

  # Data structure for format_all
  all_data = list(.by='Slide ID',
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
                    list(phenotype = "CD8+", expression = "Membrane PD1 (Opal 650) Mean"),
                    list(phenotype = "CD68+", expression = "Membrane PDL1 (Opal 520) Mean"),
                    list(phenotype = "FoxP3+", expression = 'NA'),
                    list(phenotype = "CK+", expression = "Membrane PDL1 (Opal 520) Mean"),
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
  actual_results = file.path(output_dir, 'Results.xlsx')
  expected_results = file.path(data_dir, 'Results.xlsx')

  expect_true(file.exists(file.path(output_dir, 'Script.R')))
  expect_true(file.exists(actual_results))
  expect_true(file.exists(file.path(output_dir, 'Charts.docx')))
  expect_true(file.exists(file.path(output_dir, 'nearest_neighbors.csv')))
  expect_true(file.exists(file.path(output_dir, 'count_within.csv')))
  expect_true(file.exists(file.path(output_dir, 'session_info.txt')))

  # Check numbers against known good

  sheets = excel_sheets(expected_results)
  expect_equal(excel_sheets(actual_results), sheets)

  for (sheet in sheets) {
    skip = ifelse(sheet=='H-Score', 2, 1)
    actual_sheet = read_excel(actual_results, sheet, skip=skip, .name_repair='minimal')
    expected_sheet = read_excel(expected_results, sheet, skip=skip, .name_repair='minimal')
    expect_equal(actual_sheet, expected_sheet, info=paste('Sheet name:', sheet))
  }
})
