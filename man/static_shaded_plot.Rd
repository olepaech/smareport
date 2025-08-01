% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/static shaded plot.R
\name{static_shaded_plot}
\alias{static_shaded_plot}
\title{Create a Shaded Plot of Survey Rates Over Time (ggplot2 version)}
\usage{
static_shaded_plot(
  data,
  rel_cols = c(10, 12, 14),
  xlab = "",
  ylab = "Rate (in \%)",
  title = ""
)
}
\arguments{
\item{data}{A data frame with survey response data.}

\item{rel_cols}{A vector stating in which columns of the file the data to visualize are.}

\item{xlab}{A character string specifying the x-axis label (optional).}

\item{ylab}{A character string specifying the y-axis label (optional).}

\item{title}{A character string specifying the title of the graph (optional).}
}
\value{
A ggplot2 object showing the median dfr expectation in the IQR as shaded area.
}
\description{
This function creates a static ggplot showing median values and shaded interquartile ranges
from survey response data.
}
\examples{
\dontrun{
path <- nbssma::load_participant_file()
data <- readxl::read_excel(path)
static_shaded_plot(data)
}

}
\author{
Ole Paech
}
