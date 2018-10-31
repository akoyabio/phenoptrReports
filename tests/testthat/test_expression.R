library(testthat)
library(phenoptrReports)
context("expression")

# Note: The original hard-coded test values in these tests were computed using
# variations of this snippet from the original MeanOfTop20.R:
# means = csd %>%
#   select(`Slide ID`,`Sample Name`, contains('Mean')) %>% # Select just the columns we need
#   gather('Metric', 'Value', -`Sample Name`,-`Slide ID`) %>% # Convert to a tall data frame
#   group_by(`Slide ID`,`Sample Name`, Metric) %>% # Grouping
#   top_n(20, Value) %>% # This picks the top n within each group
#   summarize(mean = mean(Value)) %>% # Compute the means
#   spread(Metric, mean) # Spread out again to wide data frame
#
# The "All" values were hand-checked.

test_that("mean expression per phenotype and tissue category calculation works", {
  csd_path = test_path('test_data', 'Consolidated_data.txt')
  csd = phenoptr::read_cell_seg_data(csd_path)
  tissue_categories = c("Tumor", "Stroma")
  phenotypes = phenoptr::parse_phenotypes('CD8+', 'CD68+', 'Total Cells')
  params = list("CD8+" = "Membrane PDL1 (Opal 520) Mean",
                "CD68+" = "Membrane PDL1 (Opal 520) Mean",
                "CD68+" = "Membrane PDL1 (Opal 520) Mean", # Duplicate s/b ignored
                "Total Cells" = "Membrane PDL1 (Opal 520) Mean")

  ex = expect_warning(
    compute_mean_expression_many(csd, phenotypes, params, tissue_categories),
    regexp='Removing duplicate')

  expect_equal(names(ex), c("Slide ID", "Tissue Category",
                            "CD8+ Membrane PDL1 (Opal 520) Mean",
                            "CD68+ Membrane PDL1 (Opal 520) Mean",
                            "Total Cells Membrane PDL1 (Opal 520) Mean"))

  expect_equal(ex$`CD8+ Membrane PDL1 (Opal 520) Mean`,
               c(0.856125, 2.03454545454545, 1.7203,
                 3.3575, 2.33060714285714, 2.55880555555556,
                 1.94514285714286, 1.48879069767442, 1.55268))

  expect_equal(ex$`CD68+ Membrane PDL1 (Opal 520) Mean`,
               c(5.96084615384615, 5.03644186046512, 5.15782828282828,
                 8.04607407407407, 6.45196590909091, 6.8262347826087,
                 4.96588888888889, 5.38612962962963, 5.32609523809524))

  expect_equal(ex$`Total Cells Membrane PDL1 (Opal 520) Mean`,
               c(1.1494983277592, 3.26520479041916, 2.16948383371825,
                 4.2125047318612, 3.57091484375, 3.78343730407523,
                 1.70227677100494, 2.2841306156406, 2.08889275843007))

  # Mean expression of top 20 cells, just check that it runs
  ex = expect_warning(
    compute_mean_expression_many(csd, phenotypes, params, tissue_categories,
                                    count=20),
    regexp='Removing duplicate')

  expect_equal(names(ex), c("Slide ID", "Tissue Category",
                            "CD8+ Top 20 Membrane PDL1 (Opal 520) Mean",
                            "CD68+ Top 20 Membrane PDL1 (Opal 520) Mean",
                            "Total Cells Top 20 Membrane PDL1 (Opal 520) Mean"))
})

test_that("mean expression with multiple markers works", {
  csd_path = test_path('test_data', 'Consolidated_data.txt')
  csd = phenoptr::read_cell_seg_data(csd_path)
  tissue_categories = c("Tumor", "Stroma")
  phenotypes = phenoptr::parse_phenotypes('CD8+', 'CD68+')
  params = list("CD8+" = "Membrane PDL1 (Opal 520) Mean",
                "CD68+" = "Membrane PDL1 (Opal 520) Mean",
                "CD68+" = "Membrane PD1 (Opal 650) Mean")

  ex = compute_mean_expression_many(csd, phenotypes, params, tissue_categories)

  expect_equal(names(ex),
               c("Slide ID", "Tissue Category",
                 "CD8+ Membrane PDL1 (Opal 520) Mean",
                 "CD68+ Membrane PDL1 (Opal 520) Mean",
                 "CD68+ Membrane PD1 (Opal 650) Mean"))

  expect_equal(ex$`CD8+ Membrane PDL1 (Opal 520) Mean`,
               c(0.856125, 2.03454545454545, 1.7203,
                 3.3575, 2.33060714285714, 2.55880555555556,
                 1.94514285714286, 1.48879069767442, 1.55268))

  expect_equal(ex$`CD68+ Membrane PDL1 (Opal 520) Mean`,
               c(5.96084615384615, 5.03644186046512, 5.15782828282828,
                 8.04607407407407, 6.45196590909091, 6.8262347826087,
                 4.96588888888889, 5.38612962962963, 5.32609523809524))

  expect_equal(ex$`CD68+ Membrane PD1 (Opal 650) Mean`,
               c(3.61384615384615, 5.70811627906977, 5.43311111111111,
                 2.37055555555556, 3.16760227272727, 2.98046956521739,
                 3.79894444444444, 5.07297222222222, 4.89096825396825))

  # Mean expression of top 20 cells, just check that it runs
  ex = compute_mean_expression_many(csd, phenotypes, params, tissue_categories,
                                    count=20)
  expect_equal(names(ex),
               c("Slide ID", "Tissue Category",
                 "CD8+ Top 20 Membrane PDL1 (Opal 520) Mean",
                 "CD68+ Top 20 Membrane PDL1 (Opal 520) Mean",
                 "CD68+ Top 20 Membrane PD1 (Opal 650) Mean"))
})

test_that('mean expression per field works', {
  csd_path = test_path('test_data', 'Consolidated_data.txt')
  csd = phenoptr::read_cell_seg_data(csd_path)
  phenotypes = phenoptr::parse_phenotypes('CD8+', 'CD68+', 'Total Cells')
  params = list("CD8+" = "Membrane PDL1 (Opal 520) Mean",
                "CD68+" = "Membrane PDL1 (Opal 520) Mean",
                "Total Cells" = "Membrane PDL1 (Opal 520) Mean")

  # If we nest csd ourselves, compute_mean_expression_many will use
  # the nesting we give it. This allows calculation by `Sample Name`
  # rather than the default `Slide ID`.
  nested = tidyr::nest(csd, -`Slide ID`, -`Sample Name`)
  ex = compute_mean_expression_many(nested, phenotypes, params, count=20)

  expect_equal(ex$`CD8+ Top 20 Membrane PDL1 (Opal 520) Mean`,
               c(2.6185, 1.32753846153846, 1.16657142857143, 4.7058, 3.78385,
                 3.17465, 2.08266666666667, 2.16594736842105, 1.1732))

  expect_equal(ex$`CD68+ Top 20 Membrane PDL1 (Opal 520) Mean`,
               c(9.00475, 6.95322222222222, 7.9204, 10.03175, 9.82136363636364,
                 13.03425, 8.7801, 10.2511, 6.62895))

  expect_equal(ex$`Total Cells Top 20 Membrane PDL1 (Opal 520) Mean`,
               c(11.39645, 17.14225, 14.12595, 24.28965, 15.91025, 32.9373,
                 10.2055, 15.4021, 11.77395))
})

test_that('mean expression by percentile works', {
  csd_path = test_path('test_data', 'Consolidated_data.txt')
  csd = phenoptr::read_cell_seg_data(csd_path)
  phenotypes = phenoptr::parse_phenotypes('CD8+', 'CD68+', 'Total Cells')
  params = list("CD8+" = "Membrane PDL1 (Opal 520) Mean",
                "CD68+" = "Membrane PDL1 (Opal 520) Mean",
                "Total Cells" = "Membrane PDL1 (Opal 520) Mean")

  # Mean expression of the bottom 10%
  ex = compute_mean_expression_many(csd, phenotypes, params, percentile=-0.1)

  expect_equal(ex$`CD8+ <= 10%ile Membrane PDL1 (Opal 520) Mean`,
               c(0.248, 0.318636363636364, 0.0584))

  expect_equal(ex$`CD68+ <= 10%ile Membrane PDL1 (Opal 520) Mean`,
               c(0.7906, 1.26833333333333, 0.771692307692308))

  expect_equal(ex$`Total Cells <= 10%ile Membrane PDL1 (Opal 520) Mean`,
               c(0.210845714285714, 0.451026041666667, 0.111851648351648))

  # Mean expression of the top 10%
  ex = compute_mean_expression_many(csd, phenotypes, params, percentile=0.9)

  expect_equal(ex$`CD8+ >= 90%ile Membrane PDL1 (Opal 520) Mean`,
               c(5.185, 7.72927272727273, 5.7702))

  expect_equal(ex$`CD68+ >= 90%ile Membrane PDL1 (Opal 520) Mean`,
               c(15.8874, 18.0736666666667, 15.4477692307692))

  expect_equal(ex$`Total Cells >= 90%ile Membrane PDL1 (Opal 520) Mean`,
               c(9.62301149425287, 16.034578125, 9.05255801104972))

  # Smoke test with tissue categories
  tissue_categories = c("Tumor", "Stroma")
  ex = compute_mean_expression_many(csd, phenotypes, params, tissue_categories,
                                    percentile=-0.1)

  expect_equal(names(ex), c("Slide ID", "Tissue Category",
                            "CD8+ <= 10%ile Membrane PDL1 (Opal 520) Mean",
                            "CD68+ <= 10%ile Membrane PDL1 (Opal 520) Mean",
                            "Total Cells <= 10%ile Membrane PDL1 (Opal 520) Mean"))
})
