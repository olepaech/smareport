#' Create a Risk Share Bar-plot (Upside or Downside)
#'
#' This function reads SMA survey data from multiple files and visualizes the composition of
#' upside or downside risk over time as a stacked bar plot.
#'
#' @param file_paths_named_list Named list of Excel file paths.
#' @param type Character string specifying risk type: either "Upside" (default) or "Downside".
#' @param infl_col A vector stating in which column of the file the inflation data are.
#' @param upside_col A vector stating in which columns of the file the upside risk data are.
#' @param downside_col A vector stating in which columns of the file the downside risk data are.
#' @param xlab A character string specifying the x-axis label (optional).
#'
#' @return A ggplot2 bar plot showing risk share distribution over time.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' file_paths <- prepare_file_list(c("May 25", "Jun 25", "Jul 25))
#' static_risk_share_bars(file_paths, type = "Upside")    #Development of Upside Risks
#' static_risk_share_bars(file_paths, type = "Downside")  #Development of Downside Risks
#' }
#'
#'
#' @export
static_risk_share_bars <- function(file_paths_named_list, type = "Upside", infl_col = c(16), upside_col = c(17:22), downside_col = c(24:29), xlab = "Survey Date") {
  suppressWarnings({
    suppressMessages({
  importance_map <- c(
    "Absolutely no relevance" = 0,
    "Not so Important" = 0.5,
    "Moderate" = 1.0,
    "Important" = 1.5,
    "Very Important" = 2.0
  )

  if (!type %in% c("Upside", "Downside")) {
    stop("type must be either 'Upside' or 'Downside'")
  }

  process_file <- function(path, label) {
    df <- readxl::read_excel(path)

    df <- df |>
      dplyr::mutate(
        Inflation = df[[infl_col]] |>
          as.character() |>
          stringr::str_replace_all("%", "") |>
          stringr::str_replace_all(",", ".") |>
          as.numeric()
      ) |>
      dplyr::select(Inflation, upside_col, downside_col) |>
      dplyr::mutate(dplyr::across(2:(length(upside_col)+length(downside_col)+1), ~ importance_map[.])) |>
      dplyr::mutate(Source = label)

    return(df)
  }

  all_data <- purrr::map2_dfr(file_paths_named_list, names(file_paths_named_list), process_file)

  all_data <- all_data |>
    dplyr::mutate(Source = factor(Source, levels = names(file_paths_named_list)))

  if (type == "Upside") {
    selected_cols <- 2:(length(upside_col)+1)
    prefix <- "Upside"
    title <- "Composition of Upside Risk Over Time"
  } else {
    selected_cols <- (length(upside_col)+2):(length(upside_col)+length(downside_col)+1)
    prefix <- "Downside"
    title <- "Composition of Downside Risk Over Time"
  }

  summary_data <- all_data |>
    dplyr::group_by(Source) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(selected_cols), ~ mean(., na.rm = TRUE), .names = paste0(prefix, "_", "{col}")))

  risk_shares <- summary_data |>
    tidyr::pivot_longer(-Source, names_to = "Risk", values_to = "Value") |>
    dplyr::mutate(
      Risk = stringr::str_remove(Risk, paste0("^", prefix, "_")),
      Risk = stringr::str_remove_all(Risk, "2$")
    ) |>
    dplyr::group_by(Source) |>
    dplyr::mutate(Share = Value / sum(Value, na.rm = TRUE)) |>
    dplyr::ungroup()

  farben <- c("#1c355e", "#0067ab", "#cce1ee", "#d15f27", "#74253e", "#00594f")
  risks <- sort(unique(risk_shares$Risk))
  farben_named <- stats::setNames(rep_len(farben, length(risks)), risks)

  p <- ggplot2::ggplot(risk_shares, ggplot2::aes(x = Source, y = Share, fill = Risk)) +
    ggplot2::geom_col(position = "fill", color = "black", width = 0.7) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = farben_named) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = paste("Share of Total", type, "Risk"),
      fill = paste(type, "Risk")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      text = ggplot2::element_text(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  return(p)
  })
  })
}
