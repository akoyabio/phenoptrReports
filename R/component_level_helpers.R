# Helpers for component levels report

# Suppress CMD CHECK notes for things that look like global vars
utils::globalVariables(c('Fluor', 'level', 'value', 'Source', 'clip_frac',
                         'x', 'y', 'pct'))

log_offset = 0.01
xlim_lower = -2

# Create a ridge plot for a single component data file
# @param path Full path to a component data file
# @param quantiles A length 2 vector of quantiles to show and report,
#   or NULL to use the default quantiles of 0.1 and 0.99.
# @return A list with plot (a ggplot object) and data (a long data frame
# containing sampled component data)
component_ridge_plot = function(path, quantiles=NULL) {
  message('Processing ', basename(path))

  if (is.null(quantiles))
    quantiles = c(0.1, 0.99)

  stopifnot(length(quantiles) == 2)

  name = basename(path) %>% stringr::str_remove('_component_data.tif')

  # Read and subsample the components and make a long data frame
  comps = phenoptr::read_components(path) %>%
    purrr::map(as.numeric) %>%
    dplyr::as_data_frame() %>%
    dplyr::sample_frac(size=0.05) %>%
    tidyr::gather('Fluor', 'value') %>%
    dplyr::mutate(Source=name)

  # This gets `quants` as a data frame with `lower` and `upper` columns
  quants = comps %>%
    tidyr::nest(-Fluor, -Source) %>%
    dplyr::mutate(q=purrr::map(data, ~quantile(.$value, quantiles)
           %>% as.list() %>% tibble::as_data_frame())) %>%
    dplyr::select(-data) %>% tidyr::unnest()

  # For plotting a long data frame is better
  quants_to_plot = quants %>%
    dplyr::select(-Source) %>%
    tidyr::gather(level, value, -Fluor)

  # How much data are we clipping (because it is 0)
  clipping = comps %>%
    dplyr::group_by(Fluor, Source) %>%
    dplyr::summarize(clip_frac=sum(value==0)/dplyr::n())

  clipping_to_plot = clipping %>%
    dplyr::mutate(
      pct=scales::percent(clip_frac, accuracy=1), x=xlim_lower+0.1, y=Inf)

  clipping_to_plot$pct[1] = paste0('Clipping: ', clipping_to_plot$pct[1])

  subtitle = stringr::str_glue(
    'Gray lines show {names(quants)[3]} and {names(quants)[4]} percentiles. Values of 0 are clipped.')

  # Some component data has very few distinct values. That makes density
  # plots look really weird, with peaks at each distinct value.
  # A histogram with lots of bins works pretty well with few or many values.
  p = ggplot2::ggplot(comps %>% dplyr::filter(value>0)) +
    ggplot2::geom_histogram(bins=1000,
                            ggplot2::aes(log10(value), color=Fluor, fill=Fluor)) +
    ggplot2::geom_vline(data=quants_to_plot,
                        ggplot2::aes(xintercept=log10(value+log_offset))) +
    ggplot2::geom_text(data=clipping_to_plot,
                       ggplot2::aes(x, y, label=pct),
              hjust=0, vjust= 1.2, size=3, fontface='bold') +
    ggplot2::facet_wrap(~Fluor, ncol=1, strip.position='left', scales='free_y') +
    ggplot2::xlim(xlim_lower, NA) +
    ggplot2::labs(x=expression(bold(log[10](Expression))), y='',
         title=name, subtitle=subtitle) +
    ggplot2::scale_color_brewer(palette='Spectral') +
    ggplot2::scale_fill_brewer(palette='Spectral') +
    ggplot2::guides(color='none', fill='none') +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.y=ggplot2::element_text(face='bold', angle=180, hjust=0),
          axis.ticks.y=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          panel.grid.major.y=ggplot2::element_blank(),
          panel.grid.minor=ggplot2::element_blank())

  list(plot=p, data=comps, quants=quants, clipping=clipping)
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
      pct=scales::percent(clip_frac, accuracy=1), x=xlim_lower+0.1, y=Inf)
  clipping_to_plot$pct[1] = paste0('Clipping: ', clipping_to_plot$pct[1])

  subtitle = stringr::str_glue(
    'Gray lines show {names(quants)[3]} and {names(quants)[4]} percentiles. Values of 0 are clipped.')

  # For plotting a long data frame is better
  quants_to_plot = quants %>%
    dplyr::select(-Fluor) %>%
    tidyr::gather(level, value, -Source)

  ggplot2::ggplot(d %>% dplyr::filter(value>0)) +
    ggplot2::geom_histogram(bins=1000,
                            ggplot2::aes(log10(value)), color=fill, fill=fill) +
    ggplot2::geom_vline(data=quants_to_plot,
                        ggplot2::aes(xintercept=log10(value+log_offset))) +
    ggplot2::geom_text(data=clipping_to_plot,
                       ggplot2::aes(x, y, label=pct),
              hjust=0, vjust= 1.2, size=3, fontface='bold') +
    ggplot2::facet_wrap(~Source, ncol=1, strip.position='left', scales='free_y') +
    ggplot2::xlim(xlim_lower, NA) +
    ggplot2::labs(x=expression(bold(log[10](Expression))), y='',
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
