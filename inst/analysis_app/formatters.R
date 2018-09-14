# Output formatting
# These functions build a script based on the user inputs

# Format everything. This is the only function in this module
# which takes reactive arguments.
format_all = function(the_data) {
  # Get values from all the phenotype modules
  phenotype_values = purrr::map(the_data$phenotypes, function(ph) ph())

  # Filter null values that happen when the controls are created,
  # then missing phenotype
  phenos = purrr::compact(phenotype_values, 'phenotype') %>%
    purrr::discard(~.x$phenotype %in% c(''))

  has_expression = any(purrr::map_lgl(phenos, ~!.x$expression %in% c('', 'NA')))
  paste0(
    format_header(),
    format_path(the_data$path),
    format_tissue_categories(the_data$tissue_categories),
    format_phenotypes(phenos),
    format_expression(phenos),
    format_trailer(the_data$path, has_expression))
}

# Initial matter
format_header = function() {
  'library(tidyverse)
library(phenoptr)
library(phenoptrReports)
library(openxlsx)\n\n'
}

# Format reading cell seg data
format_path = function(path) {
  path = stringr::str_replace_all(path, '\\\\', '/')
  stringr::str_glue('# Read the consolidated data file
csd_path = "{path}"
csd = read_tsv(csd_path)\n\n\n')
}

# Format tissue categories
format_tissue_categories = function(cats) {
  cats = cats %>% purrr::compact() %>% purrr::discard(~.x=='')
  if (length(cats)==0) return('')
  cat_str = paste(cats, collapse='", "')
  stringr::str_glue('tissue_categories = c("{cat_str}")\n\n\n')
}

# Format the phenotype definitions and counting
format_phenotypes = function(vals) {
  phenos = purrr::map_chr(vals, 'phenotype')
  if (length(phenos) == 0) return('')

  phenos = c(phenos, 'Total Cells')
  phenos_string = paste(phenos, collapse='", "')
  stringr::str_glue('# Define phenotypes
phenotypes = parse_phenotypes("{phenos_string}")

# Count phenotypes per tissue category
counts = count_phenotypes(csd, phenotypes, tissue_categories)
percents = counts_to_percents(counts)\n\n\n')
}

# Format the expression parameters
format_expression = function(vals) {
  # Filter null values that happen when the control is created,
  # then missing phenotype, then missing expression
  phenos = vals %>%
    purrr::discard(~.x$expression %in% c('', 'NA'))
  if (length(phenos) == 0) return('')

  pairs = purrr::map_chr(phenos,
                         ~stringr::str_glue('"{.x$phenotype}" = "{.x$expression}"'))
  phenos_string = paste(pairs, collapse=',\n  ')
  stringr::str_glue('# This associates phenotype names with expression
# columns to measure.
expression_params = list(
  {phenos_string}
)

# Compute mean expression per phenotype
expression_means = csd %>%
  compute_mean_expression_many(phenotypes, expression_params, tissue_categories)
\n\n')
}

format_trailer = function(path, has_expression) {
  output_path = dirname(path)
  stringr::str_glue('# This plot shows phenotype combinations
plot = upset_plot(csd)

# Write it all out to an Excel workbook
wb = createWorkbook()
write_counts_sheet(wb, counts)
write_percents_sheet(wb, percents)
write_plot_sheet(wb, plot)
{ifelse(has_expression, "write_expression_sheet(wb, expression_means)\n", "")}
saveWorkbook(wb, file.path("{output_path}", "Results.xlsx"),
  overwrite=TRUE)
')
}
