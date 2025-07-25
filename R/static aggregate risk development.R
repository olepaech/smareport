#' Aggregate Upside and Downside Risk Over Time
#'
#' Aggregates risk perceptions from multiple survey waves and returns a stacked bar plot
#' of the relative share of upside vs. downside risks over time.
#'
#' @param file_paths_named_list A named list of file paths to Excel files. The names are used as survey labels.
#' @param infl_col Index of the inflation column (default is 16).
#' @param upside_col Vector of column indices representing upside risks (default is 17:22).
#' @param downside_col Vector of column indices representing downside risks (default is 24:29).
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param title Title of the plot.
#'
#' @return A ggplot object showing the share of aggregated upside and downside risk over time.
#' 
#' @examples
#' \dontrun{
#' file_list <- nbssma::prepare_file_list(c=("May 25", "Jun 25", "Jul 25"))
#' aggregate_risk_development(file_list)
#' }
#' 
#' @author Ole Paech
#' @export
static_aggregate_risk_development <- function(file_paths_named_list,
                                       infl_col = c(16),
                                       upside_col = c(17:22),
                                       downside_col = c(24:29),
                                       xlab = "Survey Date",
                                       ylab = "Share of Total Risk",
                                       title = "Share of Aggregated Upside vs Downside Risk Over Time") {
  suppressWarnings({
    
    importance_map <- c(
      "Absolutely no relevance" = 0,
      "Not so Important" = 1,
      "Moderate" = 2,
      "Important" = 3,
      "Very Important" = 4
    )
    
    process_file <- function(path, label) {
      df <- readxl::read_excel(path)
      
      col_names <- c("Inflation",
                     paste0("Upside_", seq_along(upside_col)),
                     paste0("Downside_", seq_along(downside_col)))
      
      df <- df |>
        dplyr::mutate(
          Inflation = df[[infl_col]] |>
            as.character() |>
            stringr::str_replace_all("%", "") |>
            stringr::str_replace_all(",", ".") |>
            as.numeric()
        ) |>
        dplyr::select(Inflation,
                      dplyr::all_of(upside_col),
                      dplyr::all_of(downside_col)) |>
        stats::setNames(col_names) |>
        dplyr::mutate(dplyr::across(dplyr::starts_with("Upside_"), ~ importance_map[.])) |>
        dplyr::mutate(dplyr::across(dplyr::starts_with("Downside_"), ~ importance_map[.])) |>
        dplyr::mutate(Source = label)
      
      return(df)
    }
    
    all_data <- purrr::map2_dfr(file_paths_named_list, names(file_paths_named_list), process_file) |>
      dplyr::mutate(Source = factor(Source, levels = names(file_paths_named_list)))
    
    agg_data <- all_data |>
      dplyr::group_by(Source) |>
      dplyr::summarise(
        Upside_Sum = sum(dplyr::c_across(dplyr::starts_with("Upside_")), na.rm = TRUE),
        Downside_Sum = sum(dplyr::c_across(dplyr::starts_with("Downside_")), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Total = Upside_Sum + Downside_Sum,
        Upside_Share = Upside_Sum / Total,
        Downside_Share = Downside_Sum / Total
      ) |>
      dplyr::select(Source, Upside_Share, Downside_Share) |>
      tidyr::pivot_longer(
        cols = c("Upside_Share", "Downside_Share"),
        names_to = "Risk_Type",
        values_to = "Share"
      ) |>
      dplyr::mutate(
        Risk_Type = dplyr::case_when(
          Risk_Type == "Upside_Share" ~ "Upside Risk",
          Risk_Type == "Downside_Share" ~ "Downside Risk"
        )
      )
    
    colors <- c("Upside Risk" = "#1c355e", "Downside Risk" = "#0067ab")
    
    plot <- ggplot2::ggplot(agg_data, ggplot2::aes(x = Source, y = Share, fill = Risk_Type)) +
      ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 0.7, color = NA) +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(
        title = title,
        x = xlab,
        y = ylab,
        fill = "Risk Type"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 14),
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
      )
    
    return(plot)
  })
}
