# Helper functions

# Find the longest common prefix of a vector of strings
find_common_prefix <- function(x) {
  # Lexicographic min and max
  .min <- min(x)
  .max <- max(x)
  if (.min == .max) return (x)  # All strings are the same

  # Find the first difference by comparing characters
  .split <- strsplit(c(.min, .max), split='')
  suppressWarnings(.match <- .split[[1]] == .split[[2]])
  first_diff <- match(FALSE, .match)

  substring(x[1], 1, first_diff-1)
}

# Hmisc::escapeRegex
escapeRegex = function(string) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}
