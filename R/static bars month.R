#' Grouped Median Bar Chart Over Time
#'
#' Creates a grouped bar chart of median values over time (e.g., by month) for different
#' categories like profession, experience, or nationality. Useful for comparing response
#' distributions across groups by month.
#'
#' @param data A data frame with categorical and numeric survey response columns.
#' @param category A string: either "Profession", "Experience", or "Nationality".
#' @param rel_cols A vector of integer column indices containing numeric response values.
#' @param ylab A string specifying the y-axis label (default: "Median Rate (in %)").
#' @param title A string specifying the plot title.
#'
#' @return A ggplot object with grouped bars showing median values across time and category.
#'
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel("path")
#' static_group_bar_month(data, category = "Experience")
#' }
#' @author Ole Paech
#' @export
static_group_bar_month <- function(
    data,
    category,
    rel_cols = c(10, 12, 14),
    ylab = "Median Rate (in %)",
    title = ""
) {

  my_colors <- c("#1c355e", "#0067ab", "#cce1ee", "#a5835a", "#74253e",
                 "#00594f", "#d15f27", "#c7932c", "#a2a9ad")

  category_map <- list(
    "Profession" = "What is your profession?",
    "Experience" = "How many years of expertise do you have?",
    "Nationality" = "What is your nationality?"
  )

  if (!(category %in% names(category_map))) {
    stop("Invalid Category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
  }

  category_col <- category_map[[category]]
  relevant_cols <- names(data)[rel_cols]

  data_clean <- data |>
    dplyr::select(dplyr::all_of(category_col), dplyr::all_of(relevant_cols)) |>
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
      values_to = "Value"
    ) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::filter(!is.na(.data[[category_col]]), .data[[category_col]] != "") |>
    dplyr::mutate(Month = extract_label(Question))

  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]

  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels))

  stats <- data_long |>
    dplyr::group_by(Month, .data[[category_col]]) |>
    dplyr::summarise(
      Median = median(Value, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot2::ggplot(stats, ggplot2::aes(x = Month, y = Median, fill = .data[[category_col]])) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::scale_fill_manual(values = my_colors) +
    ggplot2::labs(
      x = "Month",
      y = ylab,
      fill = category,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      text = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.title = ggplot2::element_text(),
      legend.title = ggplot2::element_text(),
      legend.text = ggplot2::element_text()
    )
}
