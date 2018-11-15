# Helpers for background levels report

#' Create a ggridges plot for a component data file
#' @param path Full path to a component data file
#' @return A list with plot (a ggplot object) and data (a long data frame
#' containing sampled component data)
component_ridge_plot = function(path, bandwidth=0.1) {
  library(ggplot2)
  message('Processing ', basename(path))

  name = basename(path) %>% stringr::str_remove('_component_data.tif')

  # Read the components and make a long data frame
  comps = phenoptr::read_components(path) %>%
    purrr::map(as.numeric) %>%
    dplyr::as_data_frame() %>%
    dplyr::sample_frac(size=0.1) %>%
    tidyr::gather('Fluor', 'value') %>%
    mutate(Fluor=forcats::fct_rev(factor(Fluor)),
           Source=name)

  p = if (require(ggridges)){
    comps %>%
      ggplot(aes(log10(value+0.001), Fluor, fill=Fluor)) +
      geom_density_ridges(color='gray50', bandwidth=bandwidth) +
      labs(x=expression(bold(log[10](Expression))), y='', title=name) +
      scale_fill_brewer(palette='Spectral') +
      guides(color='none', fill='none') +
      theme_minimal() +
      theme(axis.text.y=element_text(face='bold', hjust=0, vjust=0)) +
      scale_x_continuous(expand = c(0.01, 0), limits=c(-2, NA)) +
      scale_y_discrete(expand = c(0.01, 0))
  } else {
    # ggplot version
    ggplot(comps, aes(log10(value+0.001), color=Fluor, fill=Fluor)) +
      geom_density(bw=bandwidth) +
      facet_wrap(~Fluor, ncol=1, strip.position='left', scales='free_y') +
      xlim(-2, NA) +
      labs(x=expression(bold(log[10](Expression))), y='', title=name) +
      scale_color_brewer(palette='Spectral') +
      scale_fill_brewer(palette='Spectral') +
      guides(color='none', fill='none') +
      theme_minimal() +
      theme(strip.text.y=element_text(face='bold', angle=180, hjust=0),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),)
  }

  list(plot=p, data=comps)
}

fluor_ridge_plot = function(d, name, fill, bandwidth=0.1) {
  library(ggplot2)
  if (require(ggridges)){
    d %>%
      ggplot(aes(log10(value+0.001), Source)) +
      geom_density_ridges(color='gray50', fill=fill, bandwidth=bandwidth) +
      labs(x=expression(bold(log[10](Expression))), y='', title=name) +
      guides(color='none', fill='none') +
      theme_minimal() +
      theme(axis.text.y=element_text(face='bold', hjust=0, vjust=0)) +
      scale_x_continuous(expand = c(0.01, 0), limits=c(-2, NA)) +
      scale_y_discrete(expand = c(0.01, 0))
  } else {
    # ggplot version
    ggplot(d, aes(log10(value+0.001))) +
      geom_density(bw=bandwidth, color='gray50', fill=fill) +
      facet_wrap(~Source, ncol=1, strip.position='left', scales='free_y') +
      xlim(-2, NA) +
      labs(x=expression(bold(log[10](Expression))), y='', title=name) +
      guides(color='none', fill='none') +
      theme_minimal() +
      theme(strip.text.y=element_text(face='bold', angle=180, hjust=0),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),)
  }
}
