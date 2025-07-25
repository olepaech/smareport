#' Static Aggregated Risk Factor Plot
#'
#' Visualizes the average importance of upside and downside risks as vertical bars
#' added to a horizontal line representing average inflation expectations. The bars
#' are placed relative to the average inflation level.
#'
#' @param df A data frame containing survey responses.
#' @param infl_col Index of the inflation column (default is 16).
#' @param upside_col Indices of upside risk columns (default is 17:22).
#' @param downside_col Indices of downside risk columns (default is 24:29).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis (default: "Average Inflation Expectation").
#' @param title Title of the plot.
#'
#' @return A ggplot object showing the relative weight of upside and downside risks around average inflation expectations.
#' @examples
#' \dontrun{
#' path <- nbssma::load_participant_files()
#' df <- readxl::read_excel(path)
#' static_aggregated_risk_factors(df)
#' }
#' @author Ole Paech
#' @export
static_aggregated_risk_factors <- function(
    df,
    infl_col = 16,
    upside_col = 17:22,
    downside_col = 24:29,
    xlab = "",
    ylab = "Average Inflation Expectation",
    title = ""
) {
  suppressWarnings({
    
    inflation_col <- names(df)[infl_col]
    risks_1 <- gsub("2", "", names(df)[upside_col])
    risks_2 <- gsub("2", "", names(df)[downside_col])
    
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
        .fns = ~ mapping[trimws(as.character(.))],
        .names = "num_{.col}"
      ))
    
    risk1_mean <- mean(unlist(df_clean |>
                                dplyr::select(dplyr::all_of(paste0("num_", risks_1)))),
                       na.rm = TRUE)
    risk2_mean <- mean(unlist(df_clean |>
                                dplyr::select(dplyr::all_of(paste0("num_", risks_2)))),
                       na.rm = TRUE)
    inflation_value <- mean(df_clean$inflation_value, na.rm = TRUE)
    
    df_plot <- data.frame(
      Group = factor(c("Upside Risks", "Downside Risks"),
                     levels = c("Upside Risks", "Downside Risks")),
      Value = c(risk1_mean, risk2_mean),
      inflation_value = inflation_value
    )
    
    farben <- c("Upside Risks" = "#1c355e", "Downside Risks" = "#0067ab")
    max_abs_risk <- max(abs(df_plot$Value))
    
    sec_breaks <- pretty(c(-max_abs_risk, max_abs_risk))
    sec_breaks_scaled <- sec_breaks / max_abs_risk
    sec_breaks_scaled <- sec_breaks_scaled[sec_breaks_scaled >= -1 & sec_breaks_scaled <= 1]
    gridlines_y <- inflation_value + sec_breaks_scaled * max_abs_risk
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = gridlines_y, color = "gray80", size = 0.3) +
      
      ggplot2::geom_rect(data = df_plot[1, ],
                         ggplot2::aes(xmin = 0.7, xmax = 1.3,
                                      ymin = inflation_value,
                                      ymax = inflation_value + Value,
                                      fill = Group),
                         color = "black") +
      ggplot2::geom_rect(data = df_plot[2, ],
                         ggplot2::aes(xmin = 0.7, xmax = 1.3,
                                      ymin = inflation_value - Value,
                                      ymax = inflation_value,
                                      fill = Group),
                         color = "black") +
      ggplot2::geom_point(ggplot2::aes(x = 1, y = inflation_value),
                          color = "#cce1ee", size = 2) +
      ggplot2::geom_hline(yintercept = inflation_value,
                          linetype = "dashed", size = 1, color = "#cce1ee") +
      ggplot2::scale_fill_manual(values = farben) +
      ggplot2::scale_x_continuous(breaks = 1, labels = "Risks") +
      ggplot2::scale_y_continuous(
        name = ylab,
        sec.axis = ggplot2::sec_axis(
          trans = ~ (. - inflation_value) / max_abs_risk,
          name = "Risk Weight",
          breaks = sec_breaks_scaled,
          labels = scales::number_format(accuracy = 0.1)(sec_breaks_scaled)
        )
      ) +
      ggplot2::labs(x = xlab, title = title, fill = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "right",
        legend.title = ggplot2::element_blank()
      )
    
    return(p)
  })
}
