#' Create facetted boxplots grouped by respondent category (ggplot2 version)
#'
#' This function takes NBS survey data and generates facetted boxplots of expectations
#' grouped by a respondent category (profession, experience, or nationality)
#' across different time periods.
#'
#' @param data A data frame containing survey results.
#' @param category A string indicating the grouping category. Must be one of
#'   `"Profession"`, `"Experience"`, or `"Nationality"`.
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A static ggplot2 object containing facetted boxplots by month and category.
#' 
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel("path")
#' static_boxplot_categories(data, category = "Experience")
#' }
#' 
#' @author Ole Paech
#' 
#' 
#' @export
static_boxplot_categories <- function(data, category, rel_cols = c(10,12,14), xlab = "", ylab = "Rate (in %)", title = "") {
  category_map <- list(
    "Profession" = "What is your profession?",
    "Experience" = "How many years of expertise do you have?",
    "Nationality" = "What is your nationality?"
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
      values_to = "Rate") |>
    dplyr::filter(!is.na(Rate)) |>
    dplyr::filter(!is.na(.data[[category_col]]), .data[[category_col]] != "") |>
    dplyr::mutate(Month = extract_label(Question))
  
  # Monatsreihenfolge anhand der rel_cols rekonstruieren
  month_levels <- unique(data_long$Month)[
    order(match(unique(data_long$Month), extract_label(relevant_cols)))
  ]
  
  data_long <- data_long |>
    dplyr::mutate(Month = factor(Month, levels = month_levels))
  
  my_colors <- c("#1c355e", "#0067ab", "#cce1ee", "#a5835a", "#74253e",
                 "#00594f", "#d15f27", "#c7932c", "#a2a9ad")
  
  ggplot2::ggplot(
    data_long,
    ggplot2::aes(x = .data[[category_col]], y = Rate, fill = .data[[category_col]])
  ) +
    ggplot2::geom_boxplot(color = "black") +
    ggplot2::facet_wrap(~ Month) +
    ggplot2::labs(x = xlab, y = ylab, fill = category, title = title) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::scale_fill_manual(values = my_colors) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial"),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "right",
      strip.text = ggplot2::element_text(family = "Arial")
    )
}
