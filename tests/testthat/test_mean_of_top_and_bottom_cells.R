library(testthat)
library(readxl)

check_top_and_bottom <- function(excel_path, csd_path, expression_cols,
                                 expected_path,
                                 tissue_categories=NULL, .by='Slide ID') {
  if (file.exists(excel_path)) file.remove(excel_path)

  mean_of_top_and_bottom_cells(csd_path, expression_cols,
                               tissue_categories=tissue_categories,
                               .by=.by, out_path=excel_path)
  expect_true(file.exists(excel_path))

  word_path = stringr::str_replace(excel_path, 'xlsx$', 'docx')
  if (file.exists(word_path)) file.remove(word_path)

  write_mean_of_top_and_bottom_charts(excel_path, word_path, .by=.by)
  expect_true(file.exists(word_path))

  # Check numbers against known good
  check_sheets(expected_path, excel_path)
}

check_sheets <- function(expected_path, excel_path) {
  sheets = excel_sheets(expected_path)
  expect_equal(excel_sheets(excel_path), sheets)

  for (sheet in sheets) {
    actual_sheet =
      read_excel(excel_path, sheet, skip=1, .name_repair='minimal')
    expected_sheet =
      read_excel(expected_path, sheet, skip=1, .name_repair='minimal')
    expect_equal(actual_sheet, expected_sheet, info=paste('Sheet name:', sheet))
  }
}

test_that("mean_of_top_and_bottom_cells works", {
  csd_path = normalizePath(test_path('test_data/Merge_cell_seg_data.txt'),
                           winslash='/', mustWork=FALSE)
  excel_path = tempfile(fileext='.xlsx')
  expected_path = test_path('test_data/Top20_Bottom10_Data.xlsx')

  expression_cols = c(
    'Nucleus DAPI Mean',
    'Membrane CD8 (Opal 480) Mean',
    'Membrane PDL1 (Opal 520) Mean',
    'Nucleus Ki67 (Opal 570) Mean',
    'Cytoplasm CD68 (Opal 620) Mean',
    'Membrane CK (Opal 690) Mean',
    'Membrane PD1 (Opal 780) Mean'
  )

  check_top_and_bottom(excel_path, csd_path, expression_cols, expected_path)

  # Should fail with invalid column
  expression_cols = c('foo', 'bar', 'baz')
  expect_error(mean_of_top_and_bottom_cells(csd_path, expression_cols),
               'foo, bar, baz')
})


# Check with .by and tissue_category
test_that("mean_of_top_and_bottom_cells works part 2", {
  csd_path = normalizePath(test_path('test_data/FIHC4_merge_cell_seg_data.txt'),
                           winslash='/', mustWork=FALSE)
  excel_path = tempfile(fileext='.xlsx')
  expected_path = normalizePath(test_path('test_data/FIHC4_Top20_Bottom10.xlsx'))

  expression_cols = c(
    'Nucleus Cy3 Mean',
    'Cytoplasm FITC Mean'
  )

  check_top_and_bottom(excel_path, csd_path, expression_cols, expected_path,
                       tissue_categories=c('Tumor', 'Stroma'),
                       .by='Sample Name')
})
