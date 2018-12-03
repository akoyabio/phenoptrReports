# Helpers for component levels report

log_offset = 0.01
xlim_lower = -2

#' Create a ridge plot for a single component data file
#' @param path Full path to a component data file
#' @return A list with plot (a ggplot object) and data (a long data frame
#' containing sampled component data)
component_ridge_plot = function(path, quantiles=c(0.1, 0.99)) {
  library(ggplot2)
  message('Processing ', basename(path))

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
  quants = comps %>% tidyr::nest(-Fluor, -Source) %>%
    dplyr::mutate(q=purrr::map(data, ~quantile(.$value, quantiles)
           %>% as.list() %>% dplyr::as_data_frame())) %>%
    dplyr::select(-data) %>% tidyr::unnest()

  # For plotting a long data frame is better
  quants_to_plot = quants %>% dplyr::select(-Source) %>%
    tidyr::gather(level, value, -Fluor)

  # How much data are we clipping (because it is 0)
  clipping = comps %>% group_by(Fluor, Source) %>%
    summarize(clip_frac=sum(value==0)/n())

  clipping_to_plot = clipping %>% mutate(
    pct=scales::percent(clip_frac, accuracy=1), x=xlim_lower+0.1, y=Inf)
  clipping_to_plot$pct[1] = paste0('Clipping: ', clipping_to_plot$pct[1])

  subtitle = stringr::str_glue(
    'Gray lines show {names(quants)[3]} and {names(quants)[4]} percentiles. Values of 0 are clipped.')

  # Some component data has very few distinct values. That makes density
  # plots look really weird, with peaks at each distinct value.
  # A histogram with lots of bins works pretty well with few or many values.
  p = ggplot(comps %>% filter(value>0)) +
    geom_histogram(bins=1000,aes(log10(value), color=Fluor, fill=Fluor)) +
    geom_vline(data=quants_to_plot, aes(xintercept=log10(value+log_offset))) +
    geom_text(data=clipping_to_plot, aes(x, y, label=pct),
              hjust=0, vjust= 1.2, size=3, fontface='bold') +
    facet_wrap(~Fluor, ncol=1, strip.position='left', scales='free_y') +
    xlim(xlim_lower, NA) +
    labs(x=expression(bold(log[10](Expression))), y='',
         title=name, subtitle=subtitle) +
    scale_color_brewer(palette='Spectral') +
    scale_fill_brewer(palette='Spectral') +
    guides(color='none', fill='none') +
    theme_minimal() +
    theme(strip.text.y=element_text(face='bold', angle=180, hjust=0),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())

  list(plot=p, data=comps, quants=quants, clipping=clipping)
}

#' Create a ridge plot for a single fluor in multiple fields
#' @param d Data for the fluor
#' @param quants Quantiles for the fluor data
#' @param clipping Clipping fractions for the fluor
#' @param name Fluor name
#' @param fill Fill color for the plot
fluor_ridge_plot = function(d, quants, clipping, name, fill) {
  library(ggplot2)
  message('Processing ', name)

  clipping_to_plot = clipping %>% mutate(
    pct=scales::percent(clip_frac, accuracy=1), x=xlim_lower+0.1, y=Inf)
  clipping_to_plot$pct[1] = paste0('Clipping: ', clipping_to_plot$pct[1])

  subtitle = stringr::str_glue(
    'Gray lines show {names(quants)[3]} and {names(quants)[4]} percentiles. Values of 0 are clipped.')

  # For plotting a long data frame is better
  quants_to_plot = quants %>% dplyr::select(-Fluor) %>%
    tidyr::gather(level, value, -Source)

  ggplot(d %>% filter(value>0)) +
    geom_histogram(bins=1000, aes(log10(value)), color=fill, fill=fill) +
    geom_vline(data=quants_to_plot, aes(xintercept=log10(value+log_offset))) +
    geom_text(data=clipping_to_plot, aes(x, y, label=pct),
              hjust=0, vjust= 1.2, size=3, fontface='bold') +
    facet_wrap(~Source, ncol=1, strip.position='left', scales='free_y') +
    xlim(xlim_lower, NA) +
    labs(x=expression(bold(log[10](Expression))), y='',
         title=name, subtitle=subtitle) +
    guides(color='none', fill='none') +
    theme_minimal() +
    theme(strip.text.y=element_text(face='bold', size=rel(0.9), angle=180, hjust=0),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
}
