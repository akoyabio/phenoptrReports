# Helper functions

# Find the longest common prefix of a vector of strings
find_common_prefix <- function(x) {
  # Check for no data
  if (is.null(x) || length(x)==0 || all(is.na(x)))
    return('')

  x = as.character(x)

  # Lexicographic min and max
  .min <- min(x, na.rm=TRUE)
  .max <- max(x, na.rm=TRUE)
  if (.min == .max) return (x)  # All strings are the same

  # Find the first difference by comparing characters
  .split <- strsplit(c(.min, .max), split='')
  suppressWarnings(.match <- .split[[1]] == .split[[2]])
  first_diff <- match(FALSE, .match)

  substring(x[1], 1, first_diff-1)
}
