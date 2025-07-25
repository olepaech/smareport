#' Static Boxplot for Median Expected DFR Rates
#'
#' This function creates a static boxplot visualization for selected inflation-related
#' survey columns. Percent values are cleaned, converted to numeric, and plotted by month.
#'
#' @param data A data frame containing percentage values as character strings.
#' @param rel_cols A vector of column indices to be plotted (default: c(10, 12, 14)).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis (default: "Median Rate (in %)").
#' @param title Plot title.
#'
#' @return A `ggplot2` object showing boxplots for the dfr expectation.
#'
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' df <- readxl::read_excel(path)
#' static_boxplot(data, rel_cols = c(10, 12, 14))
#' }
#'
#' @author Ole Paech
#' @export
static_boxplot <- function(data, rel_cols = c(10, 12, 14),
                           xlab = "", ylab = "Median Rate (in %)", title = "") {
  relevant_cols <- names(data)[rel_cols]
  
  data_clean <- data |>
    dplyr::select(dplyr::all_of(relevant_cols)) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ .x |>
        stringr::str_replace_all("%", "") |>
        stringr::str_replace_all(",", ".") |>
        as.numeric()
    ))
  
  data_long <- data_clean |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Question",
      values_to = "Rate"
    ) |>
    dplyr::filter(!is.na(Rate)) |>
    dplyr::mutate(Month = nbssma::extract_label(Question))
  
  month_levels <- nbssma::extract_label(relevant_cols)
  data_long$Month <- factor(data_long$Month, levels = month_levels)
  
  ggplot2::ggplot(data_long, ggplot2::aes(x = Month, y = Rate)) +
    ggplot2::geom_boxplot(fill = "#cce1ee", color = "#1c355e") +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title
    ) +
    ggplot2::theme_minimal()
}
