% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/excel_helpers.R
\name{write_expression_sheet}
\alias{write_expression_sheet}
\title{Write an expression table to an Excel workbook}
\usage{
write_expression_sheet(
  wb,
  exprs,
  sheet_name = "Mean Expression",
  sheet_title = "Mean Expression"
)
}
\arguments{
\item{wb}{An openxlsx Workbook from \link[openxlsx:createWorkbook]{openxlsx::createWorkbook}}

\item{exprs}{A data frame with columns for \verb{Slide ID}, \verb{Tissue Category},
and mean expression, such as the output of
\link{compute_mean_expression_many}. Count columns are not reported.}

\item{sheet_name}{Optional name for the worksheet.}

\item{sheet_title}{Optional title header for the table.}
}
\description{
Write a formatted cell expression table to a sheet in an Excel workbook.
}
\seealso{
Other output functions: 
\code{\link{write_count_within_sheet}()},
\code{\link{write_counts_sheet}()},
\code{\link{write_density_sheet}()},
\code{\link{write_h_score_sheet}()},
\code{\link{write_nearest_neighbor_summary_sheet}()},
\code{\link{write_percents_sheet}()},
\code{\link{write_plot_sheet}()},
\code{\link{write_summary_sheet}()}
}
\concept{output functions}
