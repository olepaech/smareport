#' Create a dotplot of rates by month and category
#'
#' This function takes NBS survey data and creates a dotplot
#' showing individual rate observations by month, colored by a
#' respondent category (profession, experience, or nationality).
#'
#' @param data A data frame containing survey data.
#' @param category A string indicating the grouping category. Must be one of
#'   `"Profession"`, `"Experience"`, or `"Nationality"`.
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A static ggplot2 plot object.
#'
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel("path")
#' static_dotplot_category(data, category = "Experience")
#' }
#'
#' @author Ole Paech
#'
#' @export
static_dotplot_category <- function(data, category, rel_cols = c(10,12,14), xlab = "", ylab = "Rate (in %)", title = "") {
  category_map <- list(
    "Profession" = "What is your profession? (optional)",
    "Experience" = "How many years of expertise do you have? (optional)",
    "Nationality" = "What is your nationality? (optional)"
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
      values_to = "Rate"
    ) |>
    dplyr::filter(!is.na(Rate)) |>
    dplyr::filter(!is.na(.data[[category_col]]), .data[[category_col]] != "") |>
    dplyr::mutate(Month = extract_label(Question))

  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]

  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels))

  my_colors <- c("#1c355e", "#0067ab", "#cce1ee", "#a5835a", "#74253e",
                 "#00594f", "#d15f27", "#c7932c", "#a2a9ad")

  ggplot2::ggplot(data_long, ggplot2::aes(
    x = Month,
    y = Rate,
    color = .data[[category_col]]
  )) +
    ggplot2::geom_point(size = 3, alpha = 1, position = ggplot2::position_dodge2(width = 0.7, preserve = "total", padding = 0)) +
    ggplot2::scale_color_manual(values = my_colors) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(
      x = xlab, y = ylab, title = title,
      color = category
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 11),
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.title = ggplot2::element_text(),
      axis.text = ggplot2::element_text()
    )
}
