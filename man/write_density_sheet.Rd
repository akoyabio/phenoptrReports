% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/excel_helpers.R
\name{write_density_sheet}
\alias{write_density_sheet}
\title{Write a density table to an Excel workbook}
\usage{
write_density_sheet(
  wb,
  densities,
  sheet_name = "Cell Densities",
  sheet_title = "Cell Densities (cells/mm2)"
)
}
\arguments{
\item{wb}{An openxlsx Workbook from \link[openxlsx:createWorkbook]{openxlsx::createWorkbook}}

\item{densities}{A data frame with columns for \verb{Slide ID}, \verb{Tissue Category},
\verb{Tissue Area} and densities, such as the output of
\link{compute_density_from_table}.}

\item{sheet_name}{Optional name for the worksheet.}

\item{sheet_title}{Optional title header for the table.}
}
\description{
Write a formatted density table to a sheet in an Excel workbook.
}
\seealso{
Other output functions: 
\code{\link{write_count_within_sheet}()},
\code{\link{write_counts_sheet}()},
\code{\link{write_expression_sheet}()},
\code{\link{write_h_score_sheet}()},
\code{\link{write_nearest_neighbor_summary_sheet}()},
\code{\link{write_percents_sheet}()},
\code{\link{write_plot_sheet}()},
\code{\link{write_summary_sheet}()}
}
\concept{output functions}
