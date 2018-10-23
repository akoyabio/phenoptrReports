# Helpers for signal-to-noise report

#' Guess a fluor name from the name of a singleplex image.
#'
#' This only works for DAPI, AF and Opal fluors
#'
#' @param path A file path
#' @return A fluor name
file_to_fluor = function(path) {
  name = basename(path)

  # Check a couple of easy special cases
  if (stringr::str_detect(name, 'DAPI')) return('DAPI')
  if (stringr::str_detect(name, 'AF')) return('Autofluorescence')

  # If Opal is in the name, find the following number
  opal_match = stringr::str_match(name, 'Opal[^\\d]{0,1}(\\d{3})[^\\d]')[1,2]

  if (is.na(opal_match)) {
    # If the name starts with three digits followed by a non-digit, use that
    opal_match = stringr::str_match(name, '^(\\d{3})[^\\d]')[1,2]
  }

  if (is.na(opal_match))
    stop("Cant' guess fluor name for ", name)

  # Fixup for 430 and 431 => 480
  if (opal_match %in% c('430', '431')) opal_match = '480'

  return(stringr::str_glue('Opal {opal_match}'))
}

#' Read a singleplex image and extract signal information
#'
#' @param path Path to a component_data.tif file
#' @param primary_fluor Component name of the signal fluor
#' @param percentile Percentile for high-expressing pixels. If omitted,
#'   the brightest n_bright pixels will be used after discarding `skip_percent`.
#' @param skip_fraction The fraction of brightest pixels that will be ignored.
#' @return A list containing
#'   - signals - The mean expression of each fluor in the selected bright pixels
#'   - sn - The expression of non-primary fluors as a fraction of the primary
#'   - primary - Component plane for the primary fluor (a large matrix)
process_file = function(path, primary_fluor,
                        percentile=NULL,
                        n_bright=null, skip_fraction=0.0001) {
  # Progress output
  message('Processing fluor ', primary_fluor, ' from ', path)

  # Read the components and get the primary one
  comps = read_components(path)

  if (!primary_fluor %in% names(comps))
    stop('Primary fluor ', primary_fluor, ' not found in component data file"', path, '"')

  primary = comps[[primary_fluor]]

  # Find the top-expressing `percentile` pixels
  if (is.null(percentile)) {
    # Find the top-expressing n_bright pixels, approximately.
    # The magic number is from Carla
    percentile = 1-n_bright/prod(dim(primary))-skip_fraction
  }

  low_cutoff = quantile(primary, percentile)
  high_cutoff = quantile(primary, 1-skip_fraction)
  mask = primary >= low_cutoff & primary < high_cutoff

  signals = purrr::map_dfc(comps, ~ dplyr::tibble(Mean=mean(.x[mask]))) %>%
    purrr::set_names(names(comps))

  sn = signals / signals[[primary_fluor]]

  signals$Primary = sn$Primary = primary_fluor
  signals$Source = sn$Source = basename(path)

  list(signals=signals, sn=sn, primary=primary)
}

