# Helpers for component levels report

# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c('Fluor', 'level', 'value', 'Source', 'clip_frac',
                         'x', 'y', 'pct'))

xlim_lower = 0.01

# Get data for a single component data file
# @param path Full path to a component data file
# @param name Component name
# @param quantiles A length 2 vector of quantiles to show and report,
#   or NULL to use the default quantiles of 0.1 and 0.99.
# @return A list containing
#  - data - a long data frame containing sampled component data
#  - quants - a data frame containing the requested quantiles for each component
#  - clipping - a data frame with the percent of pixels clipped to zero for
#    each component
get_component_level_data = function(path, name, quantiles=NULL) {
  message('Processing ', basename(path))

  if (is.null(quantiles))
    quantiles = c(0.1, 0.99)

  # Read and subsample the components and make a long data frame
  comps = phenoptr::read_components(path) %>%
    purrr::map(as.numeric) %>%
    dplyr::as_tibble() %>%
    dplyr::sample_frac(size=0.05) %>%
    tidyr::gather('Fluor', 'value') %>%
    dplyr::mutate(Source=name)

  # This gets `quants` as a data frame with columns for each quantile
  quants = comps %>%
    tidyr::nest(data=c(-Fluor, -Source)) %>%
    dplyr::mutate(q=purrr::map(data, ~quantile(.$value, quantiles) %>%
                                 as.list() %>%
                                 tibble::as_tibble())) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols=c(q))

  # How much data are we clipping (because it is 0)
  clipping = comps %>%
    dplyr::group_by(Fluor, Source) %>%
    dplyr::summarize(clip_frac=sum(value==0)/dplyr::n())

  list(data=comps, quants=quants, clipping=clipping)
}

# Create a ridge plot for a single component data file
# @param comps A long data frame containing sampled component data
# @param quants A data frame containing the requested quantiles for each component
# @param clipping A data frame with the percent of pixels clipped to zero for
#    each component
# @param name Component name
# @return A ggplot object containing the ridge plot
component_ridge_plot = function(comps, quants, clipping, name) {
  # For plotting a long data frame is better
  quants_to_plot = quants %>%
    dplyr::select(-Source) %>%
    tidyr::gather(level, value, -Fluor) %>%
    dplyr::mutate(value = pmax(value, xlim_lower))

  # Format clipping data as percent and add a plot location near the left margin
  clipping_to_plot = clipping %>%
    dplyr::mutate(
      pct=scales::percent(clip_frac, accuracy=1), x=xlim_lower*10^0.1, y=Inf)

  clipping_to_plot$pct[1] = paste0('Clipping: ', clipping_to_plot$pct[1])

  subtitle = 'Values of 0 are clipped.'
  if (ncol(quants) > 2)
    subtitle = stringr::str_glue(
      'Vertical lines show {cc_and(names(quants)[-(1:2)])} percentiles. {subtitle}')

  palette = discrete_colors(dplyr::n_distinct(comps$Fluor))

  # Some component data has very few distinct values. That makes density
  # plots look really weird, with peaks at each distinct value.
  # A histogram with lots of bins works pretty well with few or many values.
  ggplot2::ggplot(comps %>% dplyr::filter(value>0)) +
    ggplot2::geom_histogram(bins=1000, na.rm=TRUE,
                            ggplot2::aes(value, color=Fluor, fill=Fluor)) +
    ggplot2::geom_vline(data=quants_to_plot,
                        ggplot2::aes(xintercept=value)) +
    ggplot2::geom_text(data=clipping_to_plot,
                       ggplot2::aes(x, y, label=pct),
              hjust=0, vjust= 1.2, size=3, fontface='bold') +
    ggplot2::facet_wrap(~Fluor, ncol=1,
                        strip.position='left', scales='free_y') +
    ggplot2::scale_x_log10(limits=c(xlim_lower, NA)) +
    ggplot2::labs(x='Pixel intensity', y='',
         title=name, subtitle=subtitle) +
    ggplot2::scale_color_manual(values=palette) +
    ggplot2::scale_fill_manual(values=palette) +
    ggplot2::guides(color='none', fill='none') +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.y=ggplot2::element_text(face='bold', size=ggplot2::rel(0.9), hjust=0),
          axis.ticks.y=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          panel.grid.major.y=ggplot2::element_blank(),
          panel.grid.minor=ggplot2::element_blank())

  }

# Create a ridge plot for a single fluor in multiple fields
# @param d Data for the fluor
# @param quants Quantiles for the fluor data
# @param clipping Clipping fractions for the fluor
# @param name Fluor name
# @param fill Fill color for the plot
# @return A ggplot2 object
fluor_ridge_plot = function(d, quants, clipping, name, fill) {
  message('Processing ', name)

  clipping_to_plot = clipping %>%
    dplyr::mutate(
      pct=scales::percent(clip_frac, accuracy=1), x=xlim_lower*10^0.1, y=Inf)
  clipping_to_plot$pct[1] = paste0('Clipping: ', clipping_to_plot$pct[1])

  subtitle = 'Values of 0 are clipped.'
  if (length(quants) > 0)
    subtitle = stringr::str_glue(
    'Vertical lines show {cc_and(names(quants)[-(1:2)])} percentiles. {subtitle}')

  # For plotting a long data frame is better
  quants_to_plot = quants %>%
    dplyr::select(-Fluor) %>%
    tidyr::gather(level, value, -Source) %>%
    dplyr::mutate(value = pmax(value, xlim_lower))

  ggplot2::ggplot(d %>% dplyr::filter(value>0)) +
    ggplot2::geom_histogram(bins=1000, na.rm=TRUE,
                            ggplot2::aes(value), color=fill, fill=fill) +
    ggplot2::geom_vline(data=quants_to_plot,
                        ggplot2::aes(xintercept=value)) +
    ggplot2::geom_text(data=clipping_to_plot,
                       ggplot2::aes(x, y, label=pct),
              hjust=0, vjust= 1.2, size=3, fontface='bold') +
    ggplot2::facet_wrap(~Source, ncol=1,
                        strip.position='left', scales='free_y') +
    ggplot2::scale_x_log10(limits=c(xlim_lower, NA)) +
    ggplot2::labs(x='Pixel intensity', y='',
         title=name, subtitle=subtitle) +
    ggplot2::guides(color='none', fill='none') +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.y=ggplot2::element_text(face='bold', size=ggplot2::rel(0.9),
                                         angle=180, hjust=0),
          axis.ticks.y=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          panel.grid.major.y=ggplot2::element_blank(),
          panel.grid.minor=ggplot2::element_blank())
}

# Concatenate with comma and "and"
# @param s String vector
# @return A string
cc_and = function(s) {
  ret = paste(s[-length(s)], collapse=', ')
  if (length(s) > 1)
    ret = paste(ret, s[length(s)], sep=' and ')
  else
    ret = s
  ret
}
