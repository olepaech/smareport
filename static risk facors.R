#' Static Risk Factor Plot for Inflation Expectations
#'
#' This function visualizes the contribution of upside and downside risks
#' to the average inflation expectation. It computes weighted contributions based on
#' qualitative risk assessments and overlays them on a central inflation anchor.
#'
#' @param df A data frame with inflation expectations and qualitative risk assessments.
#' @param infl_col Integer index of the column containing inflation expectations (default: 16).
#' @param upside_col Vector of column indices for upside risks (default: 17:22).
#' @param downside_col Vector of column indices for downside risks (default: 24:29).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis (default: "Average Inflation Expectation").
#' @param title Plot title.
#'
#' @return A `ggplot2` object showing a stacked contribution plot.
#'
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' data <- readxl::read_excel(path)
#' static_risk_factors(data)
#' }
#'
#' @author Ole Paech
#' @export
static_risk_factors <- function(
    df,
    infl_col = 16,
    upside_col = 17:22,
    downside_col = 24:29,
    xlab = "",
    ylab = "Average Inflation Expectation",
    title = ""
) {
  inflation_col <- names(df)[infl_col]
  risks_1 <- names(df)[upside_col]
  risks_2 <- names(df)[downside_col]
  
  risks_1 <- gsub("2", "", risks_1)
  risks_2 <- gsub("2", "", risks_2)
  
  mapping <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 0.5,
    "Moderate" = 1.0,
    "Important" = 1.5,
    "Very Important" = 2.0
  )
  
  df_clean <- df |>
    dplyr::mutate(
      inflation_raw = .[[inflation_col]],
      inflation_clean_char = gsub(",", ".", gsub("%", "", inflation_raw)),
      inflation_value = suppressWarnings(as.numeric(trimws(inflation_clean_char)))
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c(risks_1, risks_2)),
      .fns = ~ mapping[trimws(as.character(.x))],
      .names = "num_{.col}"
    ))
  
  risk1_means <- colMeans(
    dplyr::select(df_clean, dplyr::all_of(paste0("num_", risks_1))),
    na.rm = TRUE
  )
  risk2_means <- colMeans(
    dplyr::select(df_clean, dplyr::all_of(paste0("num_", risks_2))),
    na.rm = TRUE
  )
  inflation_value <- mean(df_clean$inflation_value, na.rm = TRUE)
  
  df_plot <- data.frame(
    Variable = c(risks_1, risks_2),
    Value = c(risk1_means, -risk2_means),
    Group = c(rep("S–W", length(risks_1)), rep("X–AB", length(risks_2)))
  )
  
  unique_vars <- unique(df_plot$Variable)
  farben <- setNames(
    c("#1C355E", "#CCE1EE", "#74253E", "#00594F", "#D15F27",
      "#c7932c", "#A2A9AD", "#FF7F0E", "#17BECF", "#9467BD")[seq_along(unique_vars)],
    unique_vars
  )
  
  df_plot$Variable <- factor(df_plot$Variable, levels = unique_vars)
  
  df_plot <- df_plot |>
    dplyr::arrange(Group, -Value) |>
    dplyr::group_by(Group) |>
    dplyr::mutate(
      ymin = cumsum(dplyr::lag(Value, default = 0)),
      ymax = ymin + Value
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ymin = inflation_value + ymin,
      ymax = inflation_value + ymax,
      x = ifelse(Group == "S–W", "Upside Risks", "Downside Risks")
    ) |>
    dplyr::group_by(Group) |>
    dplyr::mutate(share = round(abs(Value) / sum(abs(Value)) * 100, 1)) |>
    dplyr::ungroup()
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = Variable)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.7, xmax = 1.3), color = "black") +
    ggplot2::geom_hline(yintercept = inflation_value, linetype = "dashed", linewidth = 1.1) +
    ggplot2::geom_point(
      ggplot2::aes(x = 1, y = inflation_value),
      color = "white", size = 2, show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = 1,
      y = max(df_plot$ymax[df_plot$Group == "S–W"]) + 0.5,
      label = "Upside Risk",
      size = 4, fontface = "bold", hjust = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = 1,
      y = min(df_plot$ymin[df_plot$Group == "X–AB"]) - 2.2,
      label = "Downside Risk",
      size = 4, fontface = "bold", hjust = 0.5
    ) +
    ggplot2::scale_fill_manual(values = farben) +
    ggplot2::scale_y_continuous(
      name = ylab,
      breaks = seq(
        floor(min(df_plot$ymin, na.rm = TRUE)),
        ceiling(max(df_plot$ymax, na.rm = TRUE)),
        by = 0.5
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = xlab, title = title) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_blank()
    )
}
