% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/static heatmap.R
\name{static_heatmap_categories}
\alias{static_heatmap_categories}
\title{Heatmap of Median Expectations by Two Categories and Month}
\usage{
static_heatmap_categories(
  data,
  category1,
  category2,
  rel_cols = c(10, 12, 14),
  title = ""
)
}
\arguments{
\item{data}{A data frame containing categorical survey responses and numeric values.}

\item{category1}{The first category for the x-axis ("Profession", "Experience", or "Nationality").}

\item{category2}{The second category for the y-axis (must differ from category1).}

\item{rel_cols}{A vector of column indices containing numeric survey results (default: c(10,12,14)).}

\item{title}{An optional title for the plot.}
}
\value{
A \code{ggplot2} object showing heatmaps of median expectations by group combinations and month.
}
\description{
This function creates a faceted heatmap of median expectations for combinations
of two respondent categories (e.g., Profession and Experience) across different time points.
}
\examples{
\dontrun{
path <- nbssma::load_participant_files()
data <- readxl::read_excel(path)
static_heatmap_categories(data, category1 = "Experience", category2 = "Profession")
}

}
\author{
Ole Paech
}
