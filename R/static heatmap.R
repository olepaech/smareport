#' Heatmap of Median Expectations by Two Categories and Month
#'
#' This function creates a faceted heatmap of median expectations for combinations
#' of two respondent categories (e.g., Profession and Experience) across different time points.
#'
#' @param data A data frame containing categorical survey responses and numeric values.
#' @param category1 The first category for the x-axis ("Profession", "Experience", or "Nationality").
#' @param category2 The second category for the y-axis (must differ from category1).
#' @param rel_cols A vector of column indices containing numeric survey results (default: c(10,12,14)).
#' @param title An optional title for the plot.
#'
#' @return A `ggplot2` object showing heatmaps of median expectations by group combinations and month.
#'
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel(path)
#' static_heatmap_categories(data, category1 = "Experience", category2 = "Profession")
#' }
#'
#' @author Ole Paech
#' @export
static_heatmap_categories <- function(
    data,
    category1,
    category2,
    rel_cols = c(10, 12, 14),
    title = ""
) {

  category_map <- list(
    "Profession" = "What is your profession?",
    "Experience" = "How many years of expertise do you have?",
    "Nationality" = "What is your nationality?"
  )

  if (!(category1 %in% names(category_map) && category2 %in% names(category_map))) {
    stop("Invalid Category. Please choose between 'Profession', 'Experience' or 'Nationality'.")
  }

  category_col1 <- category_map[[category1]]
  category_col2 <- category_map[[category2]]
  relevant_cols <- names(data)[rel_cols]

  data_clean <- data |>
    dplyr::select(
      dplyr::all_of(category_col1),
      dplyr::all_of(category_col2),
      dplyr::all_of(relevant_cols)
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(relevant_cols),
      ~ stringr::str_replace_all(.x, "%", "") |>
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
    dplyr::filter(!is.na(.data[[category_col1]]), .data[[category_col1]] != "") |>
    dplyr::filter(!is.na(.data[[category_col2]]), .data[[category_col2]] != "") |>
    dplyr::mutate(Month = extract_label(Question))

  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]

  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels)) |>
    dplyr::group_by(.data[[category_col1]], .data[[category_col2]], Month) |>
    dplyr::summarise(
      Median_Expectation = median(Value, na.rm = TRUE),
      .groups = "drop"
    )

  levels_map <- list(
    "Experience" = c("0 - 5 years", "5 - 15 years", "over 15 years"),
    "Profession" = c("Data and Statistics", "Economics and Research", "Markets",
                     "Financial Stability and Bank Supervision", "Other"),
    "Nationality" = c("Slovak", "Non-Slovak")
  )

  data_long[[category_col1]] <- factor(
    data_long[[category_col1]],
    levels = levels_map[[category1]]
  )

  data_long[[category_col2]] <- factor(
    data_long[[category_col2]],
    levels = levels_map[[category2]]
  )

  ggplot2::ggplot(data_long, ggplot2::aes(
    x = .data[[category_col1]],
    y = .data[[category_col2]],
    fill = Median_Expectation
  )) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::facet_wrap(~Month, ncol = 2) +
    ggplot2::scale_fill_gradient(low = "white", high = "#1c355e") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      x = category1,
      y = category2,
      title = title,
      fill = "Median"
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, family = "Arial"),
      axis.text.y = ggplot2::element_text(family = "Arial"),
      axis.title = ggplot2::element_text(family = "Arial"),
      plot.title = ggplot2::element_text(family = "Arial"),
      panel.spacing = grid::unit(2, "lines")
    )
}
