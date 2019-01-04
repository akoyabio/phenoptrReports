# phenoptrReports 0.1.4.9000

New features:
- The "Merge cell seg files" addin now supports searching sub-directories
  for target files.
- More informative error message if consolidation fails due to mis-matched
  data files.
  
Bug fix:
- `compute_density` works with "square microns" spelled as inForm does.

# phenoptrReports 0.1.4
**2018-12-04**

New features:
- Component levels report shows signal levels of bright and dark pixels
  in multiplex images.
  
Misc:
- Unmixing quality report works when DAPI and AF components are not present.
- `consolidate_and_summarize_cell_seg_data` processes files pairwise
  for reduced memory requirements.
- Better error handling in `consolidate_and_summarize_cell_seg_data`.
- Removed `consolidate_and_split_cell_seg_data`

# phenoptrReports 0.1.3
**2018-11-05**

New features:
- New RStudio Addin - "Merge cell seg files" merges inForm output from 
  individual fields, similar to the inForm Merge tab.
- `merge_cell_seg_files` function performs the merge.

Misc:
- Update the link to Phenoptics home page.
- `compute_mean_expression_many` ignores duplicate parameter definitions 
  instead of stopping with an obscure error.
- `compute_density_from_cell_summary` doesn't require a `Phenotype` column
  in the cell seg summary data.
- `split_phenotypes` doesn't require a `Confidence` column in the source data.
- `split_phenotypes` recognizes whitespace as a phenotype separator.

# phenoptrReports 0.1.2
**2018-10-26**

New features:
- New unmixing quality report.
- Mean expression sheet includes tissue category "All" with mean expression
  across all requested categories.
- Chart report includes count and density charts with truncated Y-axis 
  if any count or density > 2000.
  
# phenoptrReports 0.1.1.0
**2018-10-22**

New features:
- Add version number, date stamp and doco link to generated script file #2
- Add H-Score chart to the generated charts
- Add Akoya logo to footer in chart report

Bug fixes:
- Fix order of slides in Mean Expression report
- Don't crash cell seg summary report when a slide has only one phenotype
- Correctly report number of fields for data derived from a whole-slide scan

# phenoptrReports 0.1.0.0
**2018-10-15**

- Initial release.



