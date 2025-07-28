#' Create a dotplot of rates by month
#'
#' This function takes survey data and creates a dotplot
#' showing individual rate observations grouped by month.
#' The points are arranged neatly next to each other without jitter.
#'
#' @param data A data frame containing survey data.
#' @param rel_cols Integer vector indicating which columns contain the rate data.
#'   Defaults to c(10, 12, 14).
#' @param xlab Character string for the x-axis label. Defaults to `"Month"`.
#' @param ylab Character string for the y-axis label. Defaults to `"Rate (in %)"`.
#' @param title Character string for the plot title. Defaults to `""`.
#'
#' @return A ggplot2 plot object.
#'
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel(path)
#' static_dotplot(data)
#' }
#'
#' @author Ole Paech
#'
#' @export
static_dotplot <- function(data, rel_cols = c(10,12,14), xlab = "Month", ylab = "Rate (in %)", title = "") {

  relevant_cols <- names(data)[rel_cols]

  data_clean <- data |>
    dplyr::select(dplyr::all_of(relevant_cols)) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(relevant_cols),
      ~ .x |>
        stringr::str_replace_all("%", "") |>
        stringr::str_replace_all(",", ".") |>
        as.numeric()
    ))

  data_long <- data_clean |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(relevant_cols),
      names_to = "Question",
      values_to = "Rate"
    ) |>
    dplyr::filter(!is.na(Rate)) |>
    dplyr::mutate(Month = extract_label(Question))

  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]

  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels))

  ggplot2::ggplot(data_long, ggplot2::aes(x = Month, y = Rate)) +
    ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, fill = "#0067ab", color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, family = ""),
      plot.title = ggplot2::element_text(hjust = 0.5, family = ""),
      axis.title = ggplot2::element_text(family = ""),
      axis.text = ggplot2::element_text(family = "")
    )
}
