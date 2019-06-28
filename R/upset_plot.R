# Create UpSet plots of phenotype overlaps

# Make a data set for use with UpSetR
#
# [UpSetR::upset] wants sequential columns and 0/1 data.
# This function selects the phenotype columns and converts them to 0/1
# @param d A cell seg table with multiple `Phenotype` columns
# @return A data frame with just the `Phenotype` columns, coded as 0/1.
upset_data = function(d) {
  d = d %>% dplyr::select(dplyr::starts_with('Phenotype')) %>%
    dplyr::mutate_all( ~(dplyr::case_when(
      positive(.) ~ 1,
      negative(.) ~ 0,
      TRUE ~ 0)
    )) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_remove(., 'Phenotype '))) %>%
    as.data.frame() # UpSetR fails with tibble

  # Add a junk column to work around another bug
  # Not needed in dev version of UpSetR
  # cbind(junk='junk', d)
}

#' Create an UpSet plot showing the phenotype combinations present in data.
#'
#' See http://caleydo.org/tools/upset/ for an explanation of the plot.
#' @param csd A cell seg table with multiple `Phenotype` columns
#' @param expected Optional vector of expected phenotypes. If provided,
#'   these will be highlighted in the plot.
#' @return An upset plot created with [UpSetR::upset].
#' @export
upset_plot = function(csd, expected=NULL) {
  d = upset_data(csd)

  # UpSetR fails if there is only a single positive phenotype
  # In that case just return NULL
  if (sum(colSums(d)>0) < 2)
    return(NULL)

  # If we have expected phenotypes, convert to the form
  # upset uses.
  if (is.null(expected)) queries = NULL else {
    vals = stringr::str_split(expected, '/')
    queries = purrr::map(vals, ~{
      val = .x
      if (!all(positive(val)))
        stop('upset_plot uses only positive phenotypes')
      list(query = UpSetR::intersects,
           params = as.list(stringr::str_remove(val, '\\+$')),
           active = TRUE, color="#4daf4a")
    })
  }

  # Color for unexpected phenotypes, if any
  matrix.color = ifelse(is.null(queries), '#000000', '#ff7f00')

  UpSetR::upset(d, nsets=ncol(d), nintersects=NA, queries=queries,
        #number.angles=30,
        text.scale=1.5, mb.ratio = c(0.6, 0.4),
        sets.x.label='Phenotype count', order.by='freq',
        matrix.color=matrix.color, point.size=2, line.size=1)
}

positive = function(cl) endsWith(cl, '+')
negative = function(cl) endsWith(cl, '-')
