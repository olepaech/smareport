% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/static aggregate risk factors.R
\name{static_aggregated_risk_factors}
\alias{static_aggregated_risk_factors}
\title{Static Aggregated Risk Factor Plot}
\usage{
static_aggregated_risk_factors(
  df,
  infl_col = 16,
  upside_col = 17:22,
  downside_col = 24:29,
  xlab = "",
  ylab = "Average Inflation Expectation",
  ylab2 = "Risk Weight",
  title = ""
)
}
\arguments{
\item{df}{A data frame containing survey responses.}

\item{infl_col}{Index of the inflation column (default is 16).}

\item{upside_col}{Indices of upside risk columns (default is 17:22).}

\item{downside_col}{Indices of downside risk columns (default is 24:29).}

\item{xlab}{Label for the x-axis.}

\item{ylab}{Label for the y-axis (default: "Average Inflation Expectation").}

\item{title}{Title of the plot.}
}
\value{
A ggplot object showing the relative weight of upside and downside risks around average inflation expectations.
}
\description{
Visualizes the average importance of upside and downside risks as vertical bars
added to a horizontal line representing average inflation expectations. The bars
are placed relative to the average inflation level.
}
\examples{
\dontrun{
path <- nbssma::load_participant_files()
data <- readxl::read_excel(path)
static_aggregated_risk_factors(data)
}
}
\author{
Ole Paech
}
