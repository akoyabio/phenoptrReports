#' Provide summary of two annotations
#'
#' Main function to creates/compare rounded_areas and summary of annotations
#' @param path_to_annotations_files path to a list of annotation files or a single one
#' @param path_to_write where to write the summary of annotations and rounded areas
#' @param pattern which pattern your folder has, null if not anything
#'
#'
#' @return write a data frame with two sheets, summarizing information regarding annotations
#' @export

compare_annotation = function(path_to_annotations_files,
                              path_to_write,
                              pattern = NULL) {
  list_of_samples = list.files(path_to_annotations_files,
                                full.names = TRUE,pattern=pattern,
                                recursive = F)

  rounded_areas_file = Compare_annot_rounded_areas (list_of_samples)
  annotations_summary_file = annotations_summary(list_of_samples)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "rounded_areas")
  openxlsx::addWorksheet(wb, "raw_annotations_summary")
  openxlsx::writeData(wb,
                      "rounded_areas",
                      rounded_areas_file,
                      startRow = 1,
                      keepNA = FALSE)
  openxlsx::writeData(
    wb,
    "raw_annotations_summary",
    annotations_summary_file,
    startRow = 1,
    keepNA = FALSE
  )
  path_to_write = file.path(path_to_write, "annotation_summary.xlsx")
  red_bg_style = openxlsx::createStyle(bgFill = "#F96C7A")
  openxlsx::conditionalFormatting(
    wb,
    "rounded_areas",
    cols = 5,
    rows = 2:nrow(rounded_areas_file),
    rule = 'OR($E2<>0,$E2="")',
    style = red_bg_style
  )

  openxlsx::conditionalFormatting(
    wb,
    "raw_annotations_summary",
    cols = 5,
    rows = 2:nrow(annotations_summary_file),
    rule = 'OR($E2<>0,$E2="")',
    style = red_bg_style
  )

  openxlsx::saveWorkbook(wb, path_to_write,
                         overwrite = TRUE)

}

#' Round , and compute areas for two annotations
#'
#' all annotations are rounded to 2 decimal points and areas are computed
#' @param poly annotation list
#'
#' @return sum of areas
#' @export

rounded_area = function(poly) {
  if (inherits(poly, 'sfc_MULTIPOLYGON')) {
    areas = purrr::map_dbl(poly[[1]], function(coords) {
      rounded_coords = apply(coords[[1]], 2, round, digits = 2)
      sf::st_area(sf::st_polygon(list(rounded_coords)))
    })
    sum(areas)
  } else if (inherits(poly, 'sfc_POLYGON')) {
    rounded_coords = apply(poly[[1]][[1]], 2, round, digits = 2)
    sf::st_area(sf::st_polygon(list(rounded_coords)))
  } else
    stop("Unknown polygon type: ", class(poly))
}

#' Prepare and compare the areas of two annotations
#'
#' @param list_of_samples list of annotations folders
#'
#' @return Difference of rounded areas of two annotations
#' @export

Compare_annot_rounded_areas = function(list_of_samples) {
  saved_rounded = purrr::map_dfr(list_of_samples, function(sample_dir) {
    xml_geo_file = list.files(
      sample_dir,
      pattern = 'geojson$|xml$',
      full.names = TRUE,
      recursive = T
    )

    if (!any(stringr::str_detect(xml_geo_file, "xml$"))) {
      stop(list_of_samples, " is missing xml annotation")
    }
    if (!any(stringr::str_detect(xml_geo_file, "geojson$"))) {
      stop(list_of_samples, " is missing geojson annotation")
    }
    if (length(xml_geo_file) > 2) {
      stop(
        "we cannot process more than 2 annotations in ",
        sample_dir,
        " maybe you have more than a single scan!,
        or more than a single annotation of Geojson or xml look at "
        ,
        cat(xml_geo_file, sep = "\n")
      )
    }

    purrr::map_dfr(xml_geo_file, function(file_anno) {
      from_xml_geo = phenoptr::read_tagged_rois(file_anno)
      if (length(from_xml_geo) == 0) {
        stop("probably your annotation  ", file_anno, " is broken!")
      }
      anno_type = stringr::str_extract(file_anno, 'geojson$|xml$')
      purrr::map(from_xml_geo, rounded_area) %>%
        unlist() %>%
        tibble::enframe() %>%
        dplyr::mutate(anno_type = anno_type, sample_name = sample_dir)
    })
  })

  saved_rounded_wide =
    tidyr::pivot_wider(
      saved_rounded,
      names_from = 'anno_type',
      values_from = 'value',
      values_fn = list(value = sum)
    )
  saved_rounded_wide = saved_rounded_wide %>%
    dplyr::mutate(Diff = abs(geojson - xml))

  saved_rounded_wide
}
#' Order tags
#'
#' this function order tags, it is assumed that, for instance,
#' '#IncludeInResults #TC' and  '#TC #IncludeInResults' are identical
#' @param tag_str
#'
#' @return ordered tags
#' @export

order_tags = function(tag_str) {
  stringr::str_split(tag_str, ' ') %>%
    purrr::map(sort) %>%
    purrr::map_chr(function(t)
      paste0(t, collapse = ' '))
}

#' summaries the two annotations counts and their names
#'
#' @param list_of_samples list of all annotations folders
#'
#' @return the raw annotations
#' @export

annotations_summary = function(list_of_samples) {
  All_annotation = purrr::map_dfr(list_of_samples, function(sample_dir) {
    anno_files = list.files(
      sample_dir,
      pattern = "geojson$|xml$",
      full.names = TRUE,
      recursive = T
    )
    purrr::map_dfr(anno_files, function(anno_file) {
      anno_type = stringr::str_extract(anno_file, 'geojson$|xml$')
      rois_geo = phenoptr::read_phenochart_polygons(anno_file, include_rects = FALSE)
      rois_count = rois_geo %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(tags = order_tags(tags)) %>%
        dplyr::group_by(tags) %>%
        dplyr::summarize(count = dplyr::n()) %>%
        dplyr::mutate(sample = sample_dir,
                      anno_type = anno_type)
      rois_count
    })
  })

  All_annotation_organiz = All_annotation %>%
    tidyr::pivot_wider(
      names_from = 'anno_type',
      values_from = 'count',
      values_fn = list(count = sum)
    )
  All_annotation_organiz = All_annotation_organiz %>%
    dplyr::mutate(Diff = abs(geojson -	xml))
  All_annotation_organiz
}
