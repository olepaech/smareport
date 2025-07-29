#' Create a Shaded Plot of Survey Rates Over Time (ggplot2 version)
#'
#' This function creates a static ggplot showing median values and shaded interquartile ranges
#' from survey response data.
#'
#' @param data A data frame with survey response data.
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A ggplot2 object showing the median dfr expectation in the IQR as shaded area.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_file()
#' data <- readxl::read_excel(path)
#' static_shaded_plot(data)
#' }
#'
#' @export
static_shaded_plot <- function(data, rel_cols = c(10, 12, 14), xlab = "", ylab = "Rate (in %)", title = "") {
  suppressWarnings({
    relevant_cols <- names(data)[rel_cols]

    data_clean <- data |>
      dplyr::select(dplyr::all_of(relevant_cols)) |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ .x |>
          stringr::str_replace_all("%", "") |>
          stringr::str_replace_all(",", ".") |>
          as.numeric()
      ))

    data_long <- data_clean |>
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Question", values_to = "Rate") |>
      dplyr::filter(!is.na(Rate)) |>
      dplyr::mutate(Month = extract_label(Question))

    summary_stats <- data_long |>
      dplyr::group_by(Month) |>
      dplyr::summarise(
        Median = stats::median(Rate, na.rm = TRUE),
        P25 = stats::quantile(Rate, 0.25, na.rm = TRUE),
        P75 = stats::quantile(Rate, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Month_date = lubridate::parse_date_time(Month, orders = "b Y")
      ) |>
      dplyr::arrange(Month_date) |>
      dplyr::mutate(Month_numeric = dplyr::row_number())

    ggplot2::ggplot(summary_stats, ggplot2::aes(x = Month_numeric)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = P25, ymax = P75, fill = "Interquartile Range"),
        alpha = 0.4
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = Median, color = "Median"),
        size = 1.2
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = Median, color = "Median"),
        size = 2.5
      ) +
      ggplot2::scale_x_continuous(
        breaks = summary_stats$Month_numeric,
        labels = summary_stats$Month
      ) +
      ggplot2::scale_fill_manual(
        name = NULL,
        values = c("Interquartile Range" = "lightblue")
      ) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c("Median" = "#1C355E")
      ) +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = title
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "right",
        plot.title = ggplot2::element_text(hjust = 0.5)
      )
  })
}
