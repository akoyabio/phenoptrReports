context("split_phenotypes")
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
