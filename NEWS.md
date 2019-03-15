# phenoptrReports 0.1.6.900

- More forgiving handling of duplicated records in the merge step.
  It now ignores the dups with a warning.
- Reduce the extraneous console output from the merge step.

# phenoptrReports 0.1.6
**2019-03-03**

New features:
- Analysis app optionally creates tabular summaries and heatmap visualizations
  of nearest neighbor distance and count within radius for all phenotype pairs.
- Component levels report includes pair plots of component vs component
  for each source image.
  
Bug fixes:
- Component levels report
  - Write CSV files to the export directory, not its parent.
  - Fix height of "Pixel intensity by component" charts.
- Consolidate merged files
  - Fix to recognize "Annotation ID" as the field name.
- Analyze consolidated data
  - Update for dplyr 0.8 to fix #N/A values in Total lines of 
    Cell Counts, Cell Percents and Cell Density sheets
- Clean up chart output
  - "All" tissue category is last in charts which have it, same as "Total".
  - Fix "Cell Count" axis label
  - Fix legend in H-Score chart - "name repair" in readxl version 1.2.0 
    created unexpected column names which were shown in the legend.
    
# phenoptrReports 0.1.5
**2019-01-25**

New features:
- The "Merge cell seg files" addin now supports searching sub-directories
  for target files.
- The "Consolidate and summarize" addin remembers the last directory
  for subsequent selections.
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
- Add version number, date stamp and documentation link to generated script file #2
- Add H-Score chart to the generated charts
- Add Akoya logo to footer in chart report

Bug fixes:
- Fix order of slides in Mean Expression report
- Don't crash cell seg summary report when a slide has only one phenotype
- Correctly report number of fields for data derived from a whole-slide scan

# phenoptrReports 0.1.0.0
**2018-10-15**

- Initial release.



