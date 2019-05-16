# Output formatting
# These functions build a script based on the user inputs

# List to accumulate pairs of (table name, table writing function name)
# This reduces the repetition of names and the number of conditional outputs.
# Sadly it must be at global scope to be shared between all these functions.
table_pairs = list()

# Format everything.
format_all = function(all_data) {
  phenotype_values = all_data$phenotype_values

  # Filter null values that happen when the controls are created,
  # then missing phenotype
  phenos = purrr::compact(phenotype_values, 'phenotype') %>%
    purrr::discard(~.x$phenotype %in% c(''))

  .by = all_data$by

  # Flags for various sections present
  has = list()
  has$phenotypes = length(phenos) > 0
  has$density = has$phenotypes && !is.null(all_data$summary_path)
  has$expression = any(purrr::map_lgl(phenos, ~!.x$expression %in% c('', 'NA')))
  has$h_score = !is.null(all_data$score_path)

  has$include_nearest = all_data$include_nearest && length(phenos) >= 2
  has$include_nearest_details =
    has$include_nearest && all_data$include_distance_details

  has$include_count_within = (all_data$include_count_within
    && length(all_data$radii) > 0 && length(phenos) >= 2)
  has$include_count_within_details =
    has$include_count_within && all_data$include_distance_details

  # Re-initialize
  table_pairs <<- list()

  paste0(
    format_header(),
    format_path(all_data$input_path, all_data$field_col),
    format_tissue_categories(all_data$tissue_categories),
    format_phenotypes(phenos, .by),
    ifelse(has$density, format_density(all_data$summary_path), ''),
    format_expression(phenos),
    ifelse(has$h_score, format_h_score(all_data$score_path), ''),
    ifelse(has$include_nearest,
           format_nearest_neighbors(all_data$output_dir,
                                    has$include_nearest_details), ''),
    ifelse(has$include_count_within,
           format_count_within(all_data$output_dir, all_data$radii,
                               has$include_count_within_details), ''),
    format_cleanup(all_data$slide_id_prefix, all_data$use_regex, has),
    format_trailer(all_data$output_dir, has))
}

# Initial matter
format_header = function() {
  stringr::str_glue(
'# Created by phenoptr {packageVersion("phenoptr")} and phenoptrReports {packageVersion("phenoptrReports")} on {Sys.Date()}
# http://akoyabio.github.io/phenoptr
# http://akoyabio.github.io/phenoptrReports

library(tidyverse)
library(phenoptr)
library(phenoptrReports)
library(openxlsx)
\n\n')
}

# Format reading cell seg data and making summary table
format_path = function(path, field_col) {
  table_pairs <<- c(table_pairs, list(c('summary_table', 'write_summary_sheet')))
  path = stringr::str_replace_all(path, '\\\\', '/')
  stringr::str_glue('# Read the consolidated data file
csd_path = "{path}"
csd = read_cell_seg_data(csd_path)

# Make a table summarizing the number of fields per slide
summary_table = csd %>% group_by(`Slide ID`) %>%
                    summarize(`Number of fields`=n_distinct(`{field_col}`))
\n\n')
}

# Format tissue categories
format_tissue_categories = function(cats) {
  cats = cats %>% purrr::compact() %>% purrr::discard(~.x=='')
  if (length(cats)==0) return('')
  cat_str = paste(cats, collapse='", "')
  stringr::str_glue('tissue_categories = c("{cat_str}")\n\n\n')
}

# Format the phenotype definitions and counting
format_phenotypes = function(vals, .by) {
  phenos = purrr::map_chr(vals, 'phenotype')
  if (length(phenos) == 0) return('')

  # This allows multiple expression markers per pheno
  phenos = unique(phenos)

  # Always do all cells
  if (!any(stringr::str_detect(phenos,
                               stringr::regex('Total|All', ignore_case=TRUE))))
    phenos = c(unique(phenos), 'Total Cells')

  table_pairs <<- c(table_pairs, list(
    c('counts', 'write_counts_sheet'),
    c('percents', 'write_percents_sheet')
  ))

  phenos_string = paste(phenos, collapse='", "')
  stringr::str_glue('# Define phenotypes
phenotypes = parse_phenotypes("{phenos_string}")

# Column to aggregate by
.by = "{.by}"

# Count phenotypes per tissue category
counts = count_phenotypes(csd, phenotypes, tissue_categories, .by=.by)
percents = counts_to_percents(counts)
\n\n')
}

# Format density calculation
format_density = function(summary_path) {
  table_pairs <<- c(table_pairs, list(c('densities', 'write_density_sheet')))

  stringr::str_glue(
'# Path to a cell seg summary file, used for the tissue category area
summary_path = "{summary_path}"

# Using the counts computed above and the tissue area from the summary,
# compute cell densities for each phenotype
densities = compute_density_from_cell_summary(counts, summary_path,
                                              tissue_categories, .by=.by)
\n\n')
}

# Format the expression parameters
format_expression = function(vals) {
  # Filter null values that happen when the control is created,
  # then missing phenotype, then missing expression
  phenos = vals %>%
    purrr::discard(~.x$expression %in% c('', 'NA'))
  if (length(phenos) == 0) return('')

  table_pairs <<- c(table_pairs,
                    list(c('expression_means', 'write_expression_sheet')))

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
  compute_mean_expression_many(phenotypes, expression_params,
                               tissue_categories, .by=.by)
\n\n')
}

format_h_score = function(score_path) {
  table_pairs <<- c(table_pairs, list(c('h_score', 'write_h_score_sheet')))

  stringr::str_glue(
"# Compute H-Score
score_path = '{score_path}'
h_score = compute_h_score_from_score_data(csd, score_path,
                                          tissue_categories, .by=.by)
\n\n")
}

format_nearest_neighbors = function(output_dir, include_distance_details) {
  table_pairs <<- c(table_pairs,
                    list(c('nearest_neighbors', 'write_nearest_neighbor_summary_sheet')))

  if (include_distance_details)
    stringr::str_glue(
'# Summarize nearest neighbor distances
nearest_detail_path = file.path("{output_dir}", "nearest_neighbors.csv")
nearest_neighbors = nearest_neighbor_summary(csd, phenotypes,
                                             nearest_detail_path, .by=.by)
\n\n')
  else
    stringr::str_glue(
"# Summarize nearest neighbor distances
nearest_neighbors = nearest_neighbor_summary(csd, phenotypes, .by=.by)
\n\n")
}

format_count_within = function(output_dir, radii,
                               include_count_within_details) {
  table_pairs <<- c(table_pairs,
                    list(c('count_within', 'write_count_within_sheet')))
  if (include_count_within_details)
    stringr::str_glue(
'# Summary of cells within a specific distance
radii = {deparse(radii)}
count_detail_path = file.path("{output_dir}", "count_within.csv")
count_within = count_within_summary(csd, radii, phenotypes,
                                    tissue_categories, count_detail_path, .by=.by)
\n\n')
  else
    stringr::str_glue(
"# Summary of cells within a specific distance
radii = {deparse(radii)}
count_within = count_within_summary(csd, radii, phenotypes,
                                    tissue_categories, .by=.by)
\n\n")
}

format_cleanup = function(slide_id_prefix, use_regex, has) {
  if (!has$phenotypes || is.null(slide_id_prefix) || slide_id_prefix == '')
    return('')

  # We are going to use a regex to remove the Slide ID prefix.
  # If the user did not request regex, escape any special characters in
  # the prefix they provided.
  if (!use_regex)
    slide_id_prefix = escapeRegex(slide_id_prefix)

  # Now slide_id_prefix is a valid regex. We still have to double-escape \
  # to put it into a string literal.
  slide_id_prefix = stringr::str_replace(slide_id_prefix,
                                         stringr::fixed('\\'), '\\\\')

  # Note: Don't use mutate() in cleanup(),
  # it removes attribute from h_score table
  start = stringr::str_glue("# Clean up the aggregation column
# Do this at the end or it will break merges
cleanup = function(d) {{
  by_col = ifelse(.by %in% names(d), .by, 'Slide ID')
  d[[by_col]] = str_remove(d[[by_col]], '^{slide_id_prefix}')
  d
}}
\n\n")

  # Add a cleanup call for each table
  purrr::walk(table_pairs, ~{
    start <<- stringr::str_glue("{start}{.x[[1]]} = cleanup({.x[[1]]})\n\n")
  })

  paste(start, '\n')
}

format_trailer = function(output_dir, has) {
start =
'# Write it all out to an Excel workbook
wb = createWorkbook()
'
# Add a write call for each table
purrr::walk(table_pairs, ~{
  start <<- stringr::str_glue("{start}{.x[[2]]}(wb, {.x[[1]]})\n\n")
})

end = stringr::str_glue(
'

workbook_path = file.path("{output_dir}",
                          "Results.xlsx")
if (file.exists(workbook_path)) file.remove(workbook_path)
saveWorkbook(wb, workbook_path)

# Write summary charts
charts_path = file.path("{output_dir}",
                        "Charts.docx")
if (file.exists(charts_path)) file.remove(charts_path)
write_summary_charts(workbook_path, charts_path, .by=.by)

# Save session info
info_path = file.path("{output_dir}", "session_info.txt")
write_session_info(info_path)
')

paste0(start, end)
}


# Hmisc::escapeRegex
escapeRegex = function(string) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}
