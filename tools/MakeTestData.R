# Make a set of test data from the phenoptrExamples source files

library(tidyverse)
library(phenoptr)

base_path = 'C:/Research/phenoptrExamplesData/DemoSlides/export/'

# Make a merged cell seg data file
# This is a proxy for the result of running the consolidation app
csd_files = list_cell_seg_files(base_path)
csd = map_dfr(csd_files, read_cell_seg_data)

# Make a smaller test set by subsampling within each field/phenotype/tissue_category
folds = caret::createFolds(with(csd, paste(`Sample Name`, `Tissue Category`, Phenotype)))
csd = csd[folds[[1]],]
csd = split_phenotypes(csd)
out_path = here::here('tests/testthat/test_data/')

write_tsv(csd, file.path(out_path, 'Consolidated_data.txt'))

# We need a merged summary file to go with it
# Use read_tsv to preserve exact names
summary_files = list.files(base_path, pattern='_cell_seg_data_summary.txt', full.names=TRUE)
summary_csd = map_dfr(summary_files, read_tsv)
write_tsv(summary_csd, file.path(out_path, 'Merge_cell_seg_data_summary.txt'))

