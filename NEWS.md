# phenoptrReports 0.3.3.9000

- Update the minimum R version to 4.1.0 to match phenoptr requirement

# phenoptrReports 0.3.3
**2022-01-11**

- Fix access to `find_common_prefix` in analysis app (#56)

# phenoptrReports 0.3.2
**2022-01-03**

- Fix spatial map viewer to work with older composite images 
  that are missing ImageDescription (#52)
- Update tests to work with testthat 3.1.

# phenoptrReports 0.3.1
**2021-08-24**

Spatial map viewer:
- Remove a left-over debugging command.

# phenoptrReports 0.3.0
**2021-08-18**

Mean of top 20 / bottom 10 addin:
- New addin helps to evaluate whether the staining quality is
  likely to produce good unmixing. See 
  https://akoyabio.github.io/phenoptrReports/articles/top_20_bottom_10_report.html
  for details.

Staining consistency report:
- New addin creates a report showing mean expression of a selected marker
  across multiple samples.

Analysis addin:
- Fix problem computing mean expression for phenotypes with formulas 
  in their definition #47
- Allow selecting (and reporting on) multiple score summary files #49
- Tabs with H-Score results include the scored marker name 
  (in support of #49)

Spatial map viewer:
- Add support for selecting the desired view when the exported
  composite files contain multiple views.
  
Consolidate and summarize addin:
- Add a dummy `Phenotype` column if none is present #48

Merge addin:
- Read files to merge into memory and write once, instead of appending

Component levels report:
- Works correctly when "Export data from pairs plots" is checked #50
  
# phenoptrReports 0.2.13
**2021-06-14**

Analysis addin:
- Fix problems with chart generation #46

# phenoptrReports 0.2.12
**2021-06-10**

- Fix a critical bug in the consolidate and summarize addin

# phenoptrReports 0.2.11
**2021-06-01**

Consolidate and summarize addin:
- Add support for reading (and saving) only fields needed in phenoptrReports.

Analysis addin:
- Preserve (or recreate) TMA information in the analysis output files.
- Only read cell seg fields needed for the analysis.
- Fix network file paths in `Script.R`.

Component levels report:
- Fix histograms to work with more than 11 components.
- Omit pairs plots with more than 10 components.
- Add correlation heatmaps for each file

Misc:
- Update to work with multi-schema phenotypes
- Fix report generation to work when paths contain spaces
- Require R 4.0.0 or higher for compatibility with latest rtree

# phenoptrReports 0.2.10
**2020-11-11**

Consolidate and summarize addin:
- Reduce memory use to allow consolidation of larger files

Bug fixes:
- Nearest neighbor and count within summaries work correctly 
  if detailed data is not requested.
- Correctly double-escape *all* backslashes in the slide id prefix.
- Improved error checks and error messages

# phenoptrReports 0.2.9
**2020-08-22**

Analysis addin:
- Nearest neighbors and count within are computed per tissue category and 
  for all cells in any included category (#39). 
- Include expression-based phenotype columns and measured expression columns
  in count_within.txt and nearest_neighbors.txt (#36).
- Output has nicer names for expression-based phenotypes (#36).

Spatial map viewer addin:
- Fix "Save all" to work on a Mac

# phenoptrReports 0.2.8
**2020-05-18**

Analysis addin:
- Remove summary lines from the Count Within worksheet, they are not
  correct (#35)!

Spatial map viewer addin:
- For the usual use where the client browser is running on the user's 
  computer, the "Save all" saves directly to the local file system without
  creating an intermediate zip file. This is faster and avoids transfer of a
  potentially large zip file.
  
Misc:
- Fix to work correctly with dplyr 1.0.0.

# phenoptrReports 0.2.7
**2020-04-23**

Analysis addin:
- The Count Within worksheet includes summary counts across all tissue 
  categories when multiple categories are reported.
- Fix chart report to run when "Total Cells" is the only phenotype 
  defined (#32).
- Fix chart report to handle "All Cells" correctly.
- Fix H-Score report to always include every combination of 
  Slide ID (or Sample Name) and Tissue Category.
- When computing mean expression for All tissue categories, 
  don't propagate NA. The All numbers reflect the valid tissue categories.
- Fix to work with latest `dplyr`.

Spatial map viewer addin:
- Don't require Slide ID in the source cell data file.

# phenoptrReports 0.2.6
**2020-02-25**
  
Analysis addin:
- Don't require tissue segmentation in the inForm project (#4).
- Add the ability to compute H-Score for individual phenotypes (#30). 
  When selected, additional H-Score worksheets and charts will be created in the 
  output reports.
- When "Use regular expressions" is checked, the regular expression will
  match anywhere in the Slide ID, not just the prefix.
- Fill in missing tissue categories so all slides have all categories 
  reported (#1).
- Report missing / invalid values consistently as #N/A, never as #NUM.

Merge addin:
- Fix a problem that caused data corruption when cell seg data is
  formatted with comma as decimal separator (#31).

Misc:
- Use the RStudio folder chooser when available, rather than the Windows
  native chooser. The RStudio chooser is nicer and gives access to 
  network drives (#29).

# phenoptrReports 0.2.5
**2019-10-22**

Analysis addin:
- Reverse the y-axis of heatmaps so that the identity runs 
  from the top left to bottom right. 
  
Spatial map viewer improvements:
- Add "touching cells" visualization and related data.
- When saving plots, the viewer optionally saves the associated data (#22).
- Spatial map viewer works correctly with inForm data containing 
  field coordinates (vs slide coordinates) (#16).

Consolidate and summarize addin:
- Add a Slide ID column with value "None" if none is present.

Component levels report:
- Optionally export data from pairs plots as TSV files.

Misc:
- Now requires tidyr >= 1.0.0.

# phenoptrReports 0.2.4
**2019-08-07**

Spatial map viewer improvements:
- Dramatic improvement in time to render the charts, especially with 2x2 fields.

Analysis addin:
- Save nearest neighbor and count within detail as tab-separated text files
  instead of CSV files. This allows the detail files to be used as input
  to a second run of the analysis addin.
- Improved heatmaps in Charts output:
  - Maximum of eight heatmaps per page
  - Split the facet titles so the full Slide or Annotation ID is visible
- Better `nearest_neighbor_summary` 
  - Avoid warnings from `min` and `max` and return `NA` for all values 
  when there are no examples of a phenotype (#20).

Unmixing quality report:
- Tables show components as ordered in the data, not in alphabetical order.
- Pixel intensity chart uses actual pixel intensity as labels on the x-axis, 
  rather than log10 values.

Component levels report:
- Charts use actual pixel intensity as labels on the x-axis,
  rather than log10 values.

# phenoptrReports 0.2.3
**2019-07-02**

Spatial map viewer improvements:
- Calculates nearest neighbors on the fly
- Works with data from `Consolidated_data.txt` (doesn't need `nearest_neighbors.csv`)
- Supports multiple positivity and expressions in phenotype selectors!

Component levels report:
- Allow the user to enter the quantile(s) to display on the histograms.
  If two or more quantiles are entered, include a signal-to-noise table
  in the report, showing the ratio of the highest quantile to the lowest.

Bug fixes:
- Fixed a problem that caused the analysis addin to incorrectly report 
  zero cells in a phenotype (#15).
  
# phenoptrReports 0.2.2
**2019-06-18**
 
Spatial map viewer addin:
- New addin provides a field-based viewer of nearest neighbor relationships.
  See the 
  [Visualizing spatial relationships tutorial](https://akoyabio.github.io/phenoptrReports/articles/spatial_map_viewer.html) 
  for details.

Analysis addin:
- Phenotype definitions may include valid expressions such as
  `` ~`Membrane PDL1 (Opal 520) Mean`>5 ``.

Bug fixes:
- Fix error during calculation of N/A and Total cell in cell seg summary report

# phenoptrReports 0.2.1
**2019-05-31**

Bug fixes:
- Fix calculation of N/A and Total cell count in cell seg summary report (#13)

# phenoptrReports 0.2.0
**2019-05-10**

Analysis addin:
- Add support for aggregation by a selectable column (#3). This allows
  reporting by Sample Name or Annotation ID as well as by Slide ID.
- Detailed output of nearest neighbors includes the Cell IDs of 
  nearest neighbors as provided by `phenoptr::find_nearest_neighbors`.
- Recognize and correctly read inForm data 
  which uses comma as the decimal separator. 
  (Requires phenoptr >= 0.1.6.9000; related to akoyabio/phenoptr#8).
- More robust `find_common_prefix` function works with empty and 
  numeric Slide IDs.

Bug fixes:
- Fix the way the analysis addin handles special characters in Slide ID (#9).
- Fix file chooser to work on RStudio Server.

# phenoptrReports 0.1.7

Analysis addin
- Optionally save nearest-neighbor and count-within details for each cell.
- Reverse the fill scale of the nearest neighbor heatmaps so red represents 
  closer cells.
- Write session info to `session_info.txt` in the output directory.

Unmixing quality report
- Add "Guidance" section.

Component levels report
- Show a single quantile (99.9 percentile) in the signal histograms.
- Omit Autofluorescence from the pairs plots.
- Use a common scale on the individual plots within each pairs plot to 
  avoid exaggerating small signals.
- Add "Guidance" section.
- Remove "Signal to noise" section.

Merge addin
- More forgiving handling of duplicated records.
  It now ignores the duplicates with a warning.
- Less noisy console output.

General
- Reports save temporary files in a subdirectory of the output directory (#5)
- File and directory choosers are cross-platform (#8)

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



