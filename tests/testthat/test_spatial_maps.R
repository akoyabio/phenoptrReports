test_that("nearest_neighbor_map works", {
  export_path = file.path(
    'C:/Research/phenoptrExamplesData/AACR Dana Farber Melanoma',
    'subset of diverse fields')
  skip_if_not(dir.exists(export_path))

  csd = vroom::vroom(file.path(export_path, 'Consolidated_data.txt'),
                     na='#N/A', delim='\t',
                               col_types=vroom::cols())

  field_name = "Melanoma_2_Scan1_[11940,51021]"
  phenos = phenoptr::parse_phenotypes('CD8+', 'Tumor+')

  nn = nearest_neighbor_map(csd, field_name, 1, export_path, phenos,
                           'red', 'blue', 'from_to')
  vdiffr::expect_doppelganger('from_to plot matches', nn$plot)
  expect_equal(nrow(nn$data), 225)

  nn = nearest_neighbor_map(csd, field_name, 1, export_path, phenos,
                           'red', 'blue', 'from_to', dot_size=1, add_logo=FALSE)
  vdiffr::expect_doppelganger('from_to dot-1 no logo plot matches', nn$plot)
  expect_equal(dim(nn$data), c(225, 13))

  nn = nearest_neighbor_map(csd, field_name, 1, export_path, phenos,
                           'red', 'blue', 'to_from')
  vdiffr::expect_doppelganger('to_from plot matches', nn$plot)
  expect_equal(dim(nn$data), c(347, 13))

  nn = nearest_neighbor_map(csd, field_name, 1, export_path, phenos,
                           'red', 'blue', 'mutual')
  vdiffr::expect_doppelganger('mutual plot matches', nn$plot)
  expect_equal(dim(nn$data), c(86, 13))

  nn = nearest_neighbor_map(csd, field_name, 1, export_path, phenos,
                           'red', 'blue', 'none')
  vdiffr::expect_doppelganger('none plot matches', nn$plot)
  expect_equal(nn$data, NULL)
})
