library(testthat)

test_that("make_unique_names works", {
  paths = c(
    'C:/bar/foo/merge_data.txt',
    'C:/bar/baz/merge_data.txt',
    'C:/bar/baz/merge_cd3_data.txt',
    'F:/zip/baz/merge_cd3_data.txt'
  )

  # If the names are different, just use the names
  expected = c('merge_data', 'merge_cd3_data')
  expect_equal(make_unique_names(paths[2:3]), expected)

  # If the names are the same, use the dir name as well
  expected = c('foo_merge_data', 'baz_merge_data')
  expect_equal(make_unique_names(paths[1:2]), expected)

  # If that is not enough, add a sequence number
  expected = c('foo_merge_data_1', 'baz_merge_data_2',
               'baz_merge_cd3_data_3', 'baz_merge_cd3_data_4')
  expect_equal(make_unique_names(paths), expected)
})

test_that('split_phenotypes works with multi-schema data', {
  csd_path = "C:/Research/phenoptrTestData/Multi-schema/test_multi_schema.txt"
  skip_if_not(file.exists(csd_path))

  csd = phenoptr::read_cell_seg_data(csd_path, col_select='phenoptrReports')

  # Start with three multi-schema columns
  expect_equal(sum(stringr::str_detect(names(csd), 'Phenotype')), 3)

  # Splitting creates six columns and removes the Confidence columns
  csd = split_phenotypes(csd)
  expect_equal(sum(stringr::str_detect(names(csd), 'Phenotype')), 6)
  expect_equal(sum(stringr::str_detect(names(csd), 'Confidence')), 0)
  expected_names = c("Phenotype PDL-1", "Phenotype PD-1", "Phenotype CD68",
                     "Phenotype CK", "Phenotype CD8", "Phenotype Ki67")
  expect_equal(expected_names %in% names(csd), rep(TRUE, length(expected_names)))
})
