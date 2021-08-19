export_paths = c(
#  r"(C:\Research\phenoptrExamplesData\AZ R&D sharing\fixed_cd8_cd68_ck_cell_seg_data.txt)",
#  r"(C:\Research\phenoptrExamplesData\AZ R&D sharing\fixed_ki67_cell_seg_data.txt)",
  r"(C:\Research\phenoptrExamplesData\AZ R&D sharing\fixed_pd1_pdl1_cell_seg_data.txt)"
)

output_dir = r"(C:\Research\phenoptrExamplesData\AZ R&D sharing\TestingTemp)"

study_dir = r"(C:\Research\phenoptrExamplesData\AZ R&D sharing\ROI_annotations)"
export_dir = r"(C:\Research\phenoptrExamplesData\AZ R&D sharing\export_ki67)"

# Normal
devtools::load_all('.'); consolidate_and_summarize_cell_seg_data(
  export_paths, output_dir, study_dir, export_dir, require_include=TRUE,
  col_select='phenoptrReports')

# No export_dir
devtools::load_all('.'); consolidate_and_summarize_cell_seg_data(
  export_paths, output_dir, study_dir, export_dir=NULL, require_include=TRUE,
  col_select='phenoptrReports')

# No study_dir or export_dir
devtools::load_all('.'); consolidate_and_summarize_cell_seg_data(
  export_paths, output_dir, col_select='phenoptrReports')

path = "C:\\Users\\kjohnson\\Downloads\\210607 path with space\\merge_cell_seg_data.txt"
d = phenoptr::read_cell_seg_data(path, col_select='phenoptrReports')
