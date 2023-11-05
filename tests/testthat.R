library(testthat)
library(phenoptrReports)
library(rstudioapi)

# set path to current script directory
setwd(dirname(getSourceEditorContext()$path))

# run test script with test data
test_check("phenoptrReports")