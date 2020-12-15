# Output formatting
# These functions build a script based on the user inputs.
# Each function returns code to be included in the script output.
# Most code snippets create a table to include in the Results workbook.
# For each table created, a pair of strings is appended to `table_pairs`.
# The first string is code to "clean" the table; the second is code
# to add the table to the final workbook.

# `table_pairs` accumulates pairs of code snippets for table cleanup and
# worksheet writing.
# It must be at global scope to be shared between all these functions.
table_pairs = list()

# Format everything.
format_all = function(all_data) {
  phenotype_values = all_data$phenotype_values

  # Filter null values that happen when the controls are created,
  # then missing phenotype
  phenos = purrr::compact(phenotype_values, 'phenotype') %>%
    purrr::discard(~.x$phenotype %in% c(''))
  names(phenos) = purrr::map_chr(phenos, 'phenotype') %>%
    phenoptr:::phenotype_names()

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

  # Code generation
  paste0(
    format_header(),
    format_path(all_data$input_path, all_data$field_col),
    format_tissue_categories(all_data$tissue_categories),
    format_phenotypes(phenos, .by),
    ifelse(has$density, format_density(all_data$summary_path), ''),
    format_expression(phenos),
    ifelse(has$h_score, format_h_score(all_data$score_path, phenos), ''),
    ifelse(has$include_nearest,
           format_nearest_neighbors(all_data$output_dir,
              has$include_nearest_details, all_data$whole_slide), ''),
    ifelse(has$include_count_within,
           format_count_within(all_data$output_dir, all_data$radii,
              has$include_count_within_details, all_data$whole_slide), ''),
    format_cleanup(all_data$slide_id_prefix, all_data$use_regex, has),
    format_trailer(all_data$output_dir, has))
}

# Front matter loads required packages
format_header = function() {
  stringr::str_glue(
  '# Created by phenoptr {packageVersion("phenoptr")}',
  ' and phenoptrReports {packageVersion("phenoptrReports")} on {Sys.Date()}
# http://akoyabio.github.io/phenoptr
# http://akoyabio.github.io/phenoptrReports

library(tidyverse)
library(phenoptr)
library(phenoptrReports)
library(openxlsx)
\n\n')
}

# Format reading cell seg data and making a summary table
format_path = function(path, field_col) {
  table_pairs <<- c(table_pairs,
                    list(c(cleanup_code('summary_table'),
                           worksheet_code('write_summary_sheet',
                                          'summary_table'))))
  path = stringr::str_replace_all(path, '\\\\', '/')
  stringr::str_glue('# Read the consolidated data file
csd_path =
  "{path}"
csd = read_cell_seg_data(csd_path, col_select="phenoptrReports")

# Make a table summarizing the number of fields per slide
summary_table = csd %>%
  group_by(`Slide ID`) %>%
  summarize(`Number of fields`=n_distinct(`{field_col}`))
\n\n')
}

# Format tissue categories
format_tissue_categories = function(cats) {
  cats = cats %>% purrr::compact() %>% purrr::discard(~.x=='')
  if (length(cats)==0) return('tissue_categories = NA\n\n\n')
  cat_str = paste(cats, collapse='", "')
  stringr::str_glue('tissue_categories = c("{cat_str}")\n\n\n')
}

# Format the phenotype definitions, phenotype counts and percentages
format_phenotypes = function(vals, .by) {
  phenos = purrr::map_chr(vals, 'phenotype')

  # This allows multiple expression markers per pheno
  phenos = unique(phenos)

  # Always do all cells
  if (!any(stringr::str_detect(phenos,
                               stringr::regex('Total|All', ignore_case=TRUE))))
    phenos = c(unique(phenos), 'Total Cells')

  table_pairs <<- c(table_pairs, list(
    c(cleanup_code('counts'),
      worksheet_code('write_counts_sheet', 'counts')),
    c(cleanup_code('percents'),
      worksheet_code('write_percents_sheet', 'percents'))
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
  table_pairs <<- c(table_pairs,
                    list(c(cleanup_code('densities'),
                           worksheet_code('write_density_sheet', 'densities'))))

  stringr::str_glue(
'# Path to a cell seg summary file, used for the tissue category area
summary_path =
  "{summary_path}"

# Using the counts computed above and the tissue area from the summary,
# compute cell densities for each phenotype
densities = compute_density_from_cell_summary(counts, summary_path,
                                              tissue_categories, .by=.by)
\n\n')
}

# Format the expression parameters and computation of mean expression
format_expression = function(vals) {
  # Filter out phenotypes with no expression requested
  phenos = vals %>%
    purrr::discard(~.x$expression %in% c('', 'NA'))
  if (length(phenos) == 0)
    return('expression_params = NULL\n\n')

  table_pairs <<- c(table_pairs,
                    list(c(cleanup_code('expression_means'),
                           worksheet_code('write_expression_sheet',
                                          'expression_means'))))

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

# Format calculation of H-Score for all cells and optional phenotypes
format_h_score = function(score_path, phenos) {
  table_pairs <<- c(table_pairs,
                    list(c(cleanup_code('h_score'),
                           worksheet_code('write_h_score_sheet', 'h_score'))))

  # First the overall H-Score
  result = stringr::str_glue(
"# Compute H-Score
score_path =
  '{score_path}'
h_score = compute_h_score_from_score_data(csd, score_path,
                                          tissue_categories, .by=.by)
\n\n")

  # Add in any optional scoring by appending to result and table_pairs
  wants_scoring = purrr::map_lgl(phenos,
                                 ~(!is.null(.x$score) && .x$score==TRUE))

  scoring_phenos = phenos[wants_scoring] %>%
    purrr::map_chr('phenotype') %>%
    names() %>%
    unique()

  # Names for the constructed data tables
  # We need to distinguish e.g. CD8+ and CD8- here so replace + and -
  # with valid characters before calling make.names.
  table_names = scoring_phenos %>%
    stringr::str_replace_all(c('\\+'='p', '-'='m')) %>%
    make.names() %>%
    stringr::str_replace_all('\\.+', '_') %>%
    { stringr::str_glue('h_score_{.}') }

  # Names for the worksheets
  tab_names = scoring_phenos %>%
    { stringr::str_glue('H-Score {.}') } %>%
    as_valid_tab_name()

  purrr::pmap(list(scoring_phenos, table_names, tab_names),
              function(pheno, table_name, tab_name) {
    table_pairs <<-
      c(table_pairs,
        list(c(cleanup_code(table_name),
               stringr::str_glue("write_h_score_sheet(wb, {table_name},
                    '{tab_name}',
                    marker='{pheno}')\n\n"))))

    result <<- stringr::str_glue(
    "{result}{table_name}=compute_h_score_from_score_data(
       csd, score_path, tissue_categories, .by=.by,
       phenotypes[['{pheno}']])
\n\n")
  })
  result
}

# Format calculation of nearest neighbors
format_nearest_neighbors = function(output_dir, include_distance_details,
                                    whole_slide) {
  table_pairs <<- c(table_pairs,
                    list(c(cleanup_code('nearest_neighbors'),
                         worksheet_code('write_nearest_neighbor_summary_sheet',
                                        'nearest_neighbors'))))

  whole_slide_str = ifelse(whole_slide, ', whole_slide=TRUE', '')

  if (include_distance_details)
    stringr::str_glue(
'# Summarize nearest neighbor distances
nearest_detail_path = file.path(
  "{output_dir}",
  "nearest_neighbors.txt")
nearest_neighbors = nearest_neighbor_summary(
  csd, phenotypes, tissue_categories, nearest_detail_path, .by=.by,
  extra_cols=expression_params{whole_slide_str})
\n\n')
  else
    stringr::str_glue(
"# Summarize nearest neighbor distances
nearest_neighbors = nearest_neighbor_summary(
  csd, phenotypes, tissue_categories, .by=.by{whole_slide_str})
\n\n")
}

# Format calculation of `count_within`
format_count_within = function(output_dir, radii,
                               include_count_within_details,
                               whole_slide) {
  table_pairs <<- c(table_pairs,
                    list(c(cleanup_code('count_within'),
                           worksheet_code('write_count_within_sheet',
                                          'count_within'))))

  whole_slide_str = ifelse(whole_slide, ', whole_slide=TRUE', '')

  if (include_count_within_details)
    stringr::str_glue(
'# Summary of cells within a specific distance
radii = {deparse(radii)}
count_detail_path = file.path(
  "{output_dir}",
  "count_within.txt")
count_within = count_within_summary(
  csd, radii, phenotypes, tissue_categories,
  count_detail_path, .by=.by, extra_cols=expression_params{whole_slide_str})
\n\n')
  else
    stringr::str_glue(
"# Summary of cells within a specific distance
radii = {deparse(radii)}
count_within = count_within_summary(csd, radii, phenotypes,
                                    tissue_categories, .by=.by{whole_slide_str})
\n\n")
}

# Format generation of cleanup function and calls to it for each table
format_cleanup = function(slide_id_prefix, use_regex, has) {
  if (!has$phenotypes || is.null(slide_id_prefix) || slide_id_prefix == '')
    return('')

  # We are going to use a regex to remove the Slide ID prefix.
  # If the user did not request regex, escape any special characters in
  # the prefix they provided and ensure that it is a prefix match.
  if (!use_regex) {
    slide_id_prefix = escapeRegex(slide_id_prefix)
    slide_id_prefix = paste0('^', slide_id_prefix)
  }

  # Now slide_id_prefix is a valid regex. We still have to double-escape \
  # to put it into a string literal.
  slide_id_prefix = stringr::str_replace_all(slide_id_prefix,
                                         stringr::fixed('\\'), '\\\\')

  # Note: Don't use mutate() in cleanup(),
  # it removes attribute from h_score table
  start = stringr::str_glue("# Clean up the aggregation column
# Do this at the end or it will break merges
cleanup = function(d) {{
  by_col = ifelse(.by %in% names(d), .by, 'Slide ID')
  d[[by_col]] = str_remove_all(d[[by_col]], '{slide_id_prefix}')
  d
}}
\n\n")

  # Add a cleanup call for each table
  purrr::walk(table_pairs, ~{
    start <<- stringr::str_glue("{start}{.x[[1]]}")
  })

  paste(start, '\n')
}

# Format writing the workbook and creating the chart report
format_trailer = function(output_dir, has) {
start =
'# Write it all out to an Excel workbook
wb = createWorkbook()
'
# Add a write call for each table
purrr::walk(table_pairs, ~{
  start <<- stringr::str_glue("{start}{.x[[2]]}")
})

end = stringr::str_glue(
'

workbook_path = file.path(
  "{output_dir}",
  "Results.xlsx")
if (file.exists(workbook_path)) file.remove(workbook_path)
saveWorkbook(wb, workbook_path)

# Write summary charts
charts_path = file.path(
  "{output_dir}",
  "Charts.docx")
if (file.exists(charts_path)) file.remove(charts_path)
write_summary_charts(workbook_path, charts_path, .by=.by)

# Save session info
info_path = file.path(
  "{output_dir}",
  "session_info.txt")
write_session_info(info_path)
')

paste0(start, end)
}

## Helper functions
# Create valid Excel worksheet tab names from the given strings
# Tab names can't be more than 31 characters and may not include any of \/*?:[]
as_valid_tab_name = function(strs) {
  strs %>%
    stringr::str_replace_all('[\\\\/*?:\\[\\]]', '_') %>%
    substr(1, 31)
}
# Create a call to `cleanup` for the given table
cleanup_code = function(table_name) {
  stringr::str_glue("{table_name} = cleanup({table_name})\n\n")
}

# Create call to write a worksheet
worksheet_code = function(worksheet_function, table_name) {
  stringr::str_glue("{worksheet_function}(wb, {table_name})\n\n")
}

# Hmisc::escapeRegex
escapeRegex = function(string) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}
