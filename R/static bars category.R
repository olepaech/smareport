#' Creates a grouped bar chart showing Median, Mean, and Mode of selected columns
#' grouped by a specified category (e.g., Profession, Experience, or Nationality).
#'
#' @param data A data frame containing categorical and numeric columns.
#' @param category A string: "Profession", "Experience", or "Nationality".
#' @param rel_cols Integer indices of the relevant numeric columns (default: c(10, 12, 14)).
#' @param ylab Label for the y-axis.
#' @param title Title for the plot.
#'
#' @return A ggplot object showing the grouped bar chart by category and month.
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel(path)
#' static_group_bar_category(data, category = "Profession")
#' }
#' @author Ole Paech
#' @export
static_group_bar_category <- function(
    data,
    category,
    rel_cols = c(10, 12, 14),
    ylab = "Median Rate (in %)",
    title = ""
) {

  category_map <- list(
    "Profession" = "What is your profession? (optional)",
    "Experience" = "How many years of expertise do you have? (optional)",
    "Nationality" = "What is your nationality? (optional)"
  )

  if (!(category %in% names(category_map))) {
    stop("Invalid category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
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
    dplyr::group_by(.data[[category_col]], Month) |>
    dplyr::summarise(
      Median = median(Value, na.rm = TRUE),
      Mean = mean(Value, na.rm = TRUE),
      Mode = modeest::mfv(Value, na_rm = TRUE)[1],
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = c("Median", "Mean", "Mode"),
      names_to = "Statistic",
      values_to = "Value"
    )

  my_colors <- c("Median" = "#1c355e", "Mean" = "#0067ab", "Mode" = "#cce1ee")

  ggplot2::ggplot(stats, ggplot2::aes(x = .data[[category_col]], y = Value, fill = Statistic)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::facet_wrap(~ Month) +
    ggplot2::scale_fill_manual(values = my_colors) +
    ggplot2::labs(
      x = category,
      y = ylab,
      fill = "Statistic",
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}
