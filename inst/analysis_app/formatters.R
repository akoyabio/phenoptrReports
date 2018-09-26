# Output formatting
# These functions build a script based on the user inputs

# Format everything.
format_all = function(all_data) {
  # Get values from all the phenotype modules
  phenotype_values = purrr::map(all_data$phenotype_modules, function(ph) ph())

  # Filter null values that happen when the controls are created,
  # then missing phenotype
  phenos = purrr::compact(phenotype_values, 'phenotype') %>%
    purrr::discard(~.x$phenotype %in% c(''))

  # Flags for various sections present
  has = list()
  has$phenotypes = length(phenos) > 0
  has$density = has$phenotypes && !is.null(all_data$summary_path)
  has$expression = any(purrr::map_lgl(phenos, ~!.x$expression %in% c('', 'NA')))
  has$h_score = !is.null(all_data$score_path)

  paste0(
    format_header(),
    format_path(all_data$input_path),
    format_tissue_categories(all_data$tissue_categories),
    format_phenotypes(phenos),
    ifelse(has$density, format_density(all_data$summary_path), ''),
    format_expression(phenos),
    ifelse(has$h_score, format_h_score(all_data$score_path), ''),
    format_cleanup(all_data$slide_id_prefix, all_data$use_regex, has),
    format_trailer(all_data$output_dir, has))
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
csd = read_cell_seg_data(csd_path)\n\n\n')
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

  # This allows multiple expression markers per pheno
  phenos = unique(phenos)

  # Always do all cells
  if (!any(stringr::str_detect(phenos, 'Total|All')))
    phenos = c(unique(phenos), 'Total Cells')

  phenos_string = paste(phenos, collapse='", "')
  stringr::str_glue('# Define phenotypes
phenotypes = parse_phenotypes("{phenos_string}")

# Count phenotypes per tissue category
counts = count_phenotypes(csd, phenotypes, tissue_categories)
percents = counts_to_percents(counts)\n\n\n')
}

# Format density calculation
format_density = function(summary_path) {
  stringr::str_glue(
'# Path to a cell seg summary file, used for the tissue category area
summary_path = "{summary_path}"

# Using the counts computed above and the tissue area from the summary,
# compute cell densities for each phenotype
densities = compute_density_from_cell_summary(counts, summary_path, tissue_categories)\n\n\n')
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
  stringr::str_glue(
'# Associate phenotype names with expression columns to measure.
expression_params = list(
  {phenos_string}
)

# Compute mean expression per phenotype
expression_means = csd %>%
  compute_mean_expression_many(phenotypes, expression_params, tissue_categories)
\n\n')
}

format_h_score = function(score_path) {
  stringr::str_glue(
"# Compute H-Score
score_path = '{score_path}'
h_score = compute_h_score_from_score_data(csd, score_path)\n\n\n")
}

format_cleanup = function(slide_id_prefix, use_regex, has) {
  if (!has$phenotypes || is.null(slide_id_prefix) || slide_id_prefix == '')
    return('')

  if (use_regex) {
    # slide_id_prefix is a regex. We just have to double-escape \
    # to put it into a string literal
    slide_id_prefix = stringr::str_replace(slide_id_prefix, fixed('\\'), '\\\\')
  } else slide_id_prefix = escapeRegex(slide_id_prefix)

  start = stringr::str_glue("# Clean up the Slide IDs
# Do this at the end or it will break merges
cleanup = function(d) {{
  d %>% mutate(`Slide ID` = str_remove(`Slide ID`, '^{slide_id_prefix}'))
}}\n\n\n")

  if (has$phenotypes)
    start = paste0(start, "counts = cleanup(counts)
percents = cleanup(percents)\n")

  if (has$density)
    start = paste0(start, "densities = cleanup(densities)\n")

  if (has$expression)
    start = paste0(start, "expression_means = cleanup(expression_means)\n")

  if (has$h_score)
    start = paste0(start, "h_score = cleanup(h_score)\n")

  paste(start, '\n\n')
}

format_trailer = function(output_dir, has) {
start =
'# This plot shows phenotype combinations
plot = upset_plot(csd)

# Write it all out to an Excel workbook
wb = createWorkbook()
'

counts = ifelse(has$phenotypes,
"write_counts_sheet(wb, counts)
write_percents_sheet(wb, percents)
", "")

plot =
"write_plot_sheet(wb, plot)
"

density = ifelse(has$density,
"write_density_sheet(wb, densities)
", "")

expression = ifelse(has$expression,
                    "write_expression_sheet(wb, expression_means)
                    ", "")

h_score = ifelse(has$h_score,
                    "write_h_score_sheet(wb, h_score)
", "")

end = stringr::str_glue(
'saveWorkbook(wb, file.path("{output_dir}", "Results.xlsx"),
  overwrite=TRUE)
')

paste0(start, counts, plot, density, expression, h_score, end)
}
