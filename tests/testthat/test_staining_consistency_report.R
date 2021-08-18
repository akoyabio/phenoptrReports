library(testthat)

test_that('staining_consistency_report works', {
  csd_path = normalizePath(test_path('test_data/Merge_cell_seg_data.txt'))
  output_path = normalizePath(test_path('results'), mustWork=FALSE)

  if (dir.exists(output_path)) unlink(output_path, recursive=TRUE)
  staining_consistency_report(csd_path, 'pdl1', 'Membrane', output_path)

  expect(file.exists(
    file.path(output_path, 'Staining_consistency_report.html')),
    'Staining_consistency_report.html not found.')
})
