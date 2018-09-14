# Output formatting

# Format tissue categories
format_tissue_categories = function(cats) {
  cats = cats %>% purrr::compact() %>% purrr::discard(~.x=='')
  if (length(cats)==0) return('')
  cat_str = paste(cats, collapse='", "')
  stringr::str_glue('tissue_categories = c("{cat_str}")\n\n')
}

# Format the phenotype definitions
format_phenotypes = function(vals) {
  phenos = purrr::map(vals, 'phenotype') %>%
    purrr::compact() %>%
    purrr::discard(~.x=='')
  if (length(phenos) == 0) return('')

  phenos_string = paste(phenos, collapse='", "')
  stringr::str_glue('phenotypes = parse_phenotypes("{phenos_string}")\n\n')
}

# Format the expression parameters
format_expression = function(vals) {
  # Filter null values that happen when the control is created,
  # then missing phenotype, then missing expression
  phenos = purrr::compact(vals, 'phenotype') %>%
    purrr::discard(~.x$phenotype %in% c('')) %>%
    purrr::discard(~.x$expression %in% c('', 'NA'))
  if (length(phenos) == 0) return('')

  pairs = purrr::map_chr(phenos,
                         ~stringr::str_glue('"{.x$phenotype}" = "{.x$expression}"'))
  phenos_string = paste(pairs, collapse=',\n  ')
  stringr::str_glue('expression_params = list(
  {phenos_string}
)\n\n')
}
