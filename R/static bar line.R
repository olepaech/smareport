#' Create a bar and line plot showing median and IQR over time (staitc version)
#'
#' @param data A data.frame with the survey data.
#' @param rel_cols A vector stating in which columns of the file the data to visualize are.
#' @param title1 A character string specifying the title of the left y-axis (optional).
#' @param title2 A character string specifying the title of the right y-axis (optional).
#'
#' @return A ggplot2 object with dual-axis style median (bar) and IQR (line) chart.
#'
#' @examples
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel(path)
#' static_bar_line(data, rel_cols = c(10,12,14), title1 = "Median (left)", title2 = "IQR (right)")
#'
#' @author Ole Paech
#'
#' @importFrom dplyr select all_of mutate across filter group_by summarise ungroup row_number arrange
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point scale_y_continuous scale_x_continuous labs theme_minimal element_text sec_axis
#' @importFrom lubridate parse_date_time
#' @importFrom stats median quantile
#'
#' @export
static_bar_line <- function(data, rel_cols = c(10,12,14), title1 = "Median in % (left)", title2 = "IQR (right)") {
  suppressWarnings({
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
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Question", values_to = "Value") |>
      dplyr::filter(!is.na(Value)) |>
      dplyr::mutate(Month = extract_label(Question))

    stats <- data_long |>
      dplyr::group_by(Month) |>
      dplyr::summarise(
        Median = stats::median(Value, na.rm = TRUE),
        P25 = stats::quantile(Value, 0.25, na.rm = TRUE),
        P75 = stats::quantile(Value, 0.75, na.rm = TRUE),
        IQR = P75 - P25,
        .groups = "drop"
      ) |>
      dplyr::mutate(Month_date = lubridate::parse_date_time(Month, orders = "b Y")) |>
      dplyr::arrange(Month_date) |>
      dplyr::mutate(Month_numeric = dplyr::row_number()) |>
      dplyr::select(-Month_date)

    max_median <- max(stats$Median, na.rm = TRUE)
    max_iqr <- max(stats$IQR, na.rm = TRUE)
    ratio <- max_median / max_iqr

    ggplot2::ggplot(stats, ggplot2::aes(x = Month_numeric)) +
      ggplot2::geom_col(
        ggplot2::aes(y = Median, fill = "Median"),
        width = 0.6, show.legend = TRUE
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = IQR * ratio, color = "IQR"),
        size = 1.5, show.legend = TRUE
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = IQR * ratio, color = "IQR"),
        size = 3, show.legend = FALSE
      ) +
      ggplot2::scale_x_continuous(
        breaks = stats$Month_numeric,
        labels = stats$Month
      ) +
      ggplot2::scale_y_continuous(
        name = title1,
        sec.axis = ggplot2::sec_axis(~./ratio, name = title2)
      ) +
      ggplot2::scale_fill_manual(
        name = NULL,
        values = c("Median" = "#1c355e")
      ) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c("IQR" = "#cce1ee")
      ) +
      ggplot2::labs(
        x = "",
        title = ""
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "right"
      )
  })
}
