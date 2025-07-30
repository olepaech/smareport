#' Visualize Historical Inflation Risk Perception (Static Plot)
#'
#' This function processes labeled Excel survey files and visualizes
#' the historical development of upside and downside inflation risk
#' in relation to inflation expectations using a stacked bar plot.
#'
#' @param files_with_labels A chronological named list, where each element is the path
#'   to an Excel file and the name is the corresponding survey date label.
#' @param infl_col A vector stating in which column of the file the inflation data are.
#' @param upside_col A vector stating in which columns of the file the upside risk data are.
#' @param downside_col A vector stating in which columns of the file the downside risk data are.
#' @param xlab A character string specifying the x-axis label (optional).
#' @param ylab A character string specifying the y-axis label (optional).
#' @param title A character string specifying the title of the graph (optional).
#'
#' @return A static ggplot2 object showing inflation expectations and risk decomposition.
#'
#' @author Ole Paech
#'
#' @examples
#' \dontrun{
#' file_list <- nbssma::prepare_file_list(c=("May 25", "Jun 25", "Jul 25"))
#' static_inflation_risk_history(file_list)
#' }
#'
#' @export
static_inflation_risk_history <- function(files_with_labels, infl_col = c(16), upside_col = c(17:22), downside_col = c(24:29), xlab = "Survey Date", ylab = "Average Inflation Expectations (in %)", title = "Development of Inflation Projections and Perceived Risks") {
  suppressWarnings({
    importance_map <- c(
      "Absolutely no relevance" = 0,
      "Not so Important" = 0.25,
      "Moderate" = 0.5,
      "Important" = 0.75,
      "Very Important" = 1.0
    )

    rename_risk <- function(x) {
      x <- gsub("Upside_", "", x)
      x <- gsub("Downside_", "", x)
      x <- gsub("2", "", x)
      return(x)
    }

    process_file <- function(path, label) {
      df <- readxl::read_excel(path)
      df <- dplyr::mutate(df,
                          Inflation = df[[infl_col]] |>
                            as.character() |>
                            stringr::str_replace_all("%", "") |>
                            stringr::str_replace_all(",", ".") |>
                            as.numeric()
      ) |>
        dplyr::select(Inflation, upside_col, downside_col) |>
        dplyr::mutate(dplyr::across(2:(length(upside_col) + length(downside_col) + 1), ~ importance_map[.])) |>
        dplyr::mutate(Source = label)

      return(df)
    }

    all_data <- dplyr::bind_rows(
      lapply(seq_along(files_with_labels), function(i) {
        process_file(files_with_labels[[i]], names(files_with_labels)[i])
      })
    ) |>
      dplyr::mutate(Source = factor(Source, levels = names(files_with_labels)))

    summary_data <- all_data |>
      dplyr::group_by(Source) |>
      dplyr::summarise(
        inflation_exp = mean(Inflation, na.rm = TRUE),
        dplyr::across(2:(length(upside_col) + 1), ~ mean(., na.rm = TRUE), .names = "Upside_{.col}"),
        dplyr::across((length(upside_col) + 2):(length(upside_col) + length(downside_col) + 1), ~ mean(., na.rm = TRUE), .names = "Downside_{.col}")
      )

    upside <- summary_data |>
      dplyr::select(Source, dplyr::starts_with("Upside_")) |>
      tidyr::pivot_longer(-Source, names_to = "Risk", values_to = "Value") |>
      dplyr::mutate(Type = "Upside", Risk_clean = rename_risk(Risk))

    downside <- summary_data |>
      dplyr::select(Source, dplyr::starts_with("Downside_")) |>
      tidyr::pivot_longer(-Source, names_to = "Risk", values_to = "Value") |>
      dplyr::mutate(Type = "Downside", Risk_clean = rename_risk(Risk))

    stack_data <- dplyr::bind_rows(upside, downside) |>
      dplyr::left_join(dplyr::select(summary_data, Source, inflation_exp), by = "Source") |>
      dplyr::group_by(Source, Type) |>
      dplyr::arrange(Source, Type, Risk_clean) |>
      dplyr::mutate(
        Share = Value / sum(Value, na.rm = TRUE),
        Total = sum(Value, na.rm = TRUE),
        cum_share = cumsum(Share),
        ymin = ifelse(Type == "Upside", inflation_exp + Total * (cum_share - Share),
                      inflation_exp - Total * cum_share),
        ymax = ifelse(Type == "Upside", inflation_exp + Total * cum_share,
                      inflation_exp - Total * (cum_share - Share))
      ) |>
      dplyr::ungroup()

    unique_risks <- sort(unique(stack_data$Risk_clean))
    colors <- c("#1C355E", "#0067AB", "#CCE1EE", "#A5835A", "#74253E", "#00594F", "#D15F27", "#C7932C", "#A2A9AD")
    if (length(unique_risks) > 9) {
      extra_colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)]
      set.seed(123)
      colors <- c(colors, sample(extra_colors, length(unique_risks) - 9))
    }
    colors_named <- setNames(colors[1:length(unique_risks)], unique_risks)

    inflation_points <- summary_data |>
      dplyr::mutate(x = as.numeric(factor(Source)))

    totals_df <- stack_data |>
      dplyr::group_by(Source, Type) |>
      dplyr::summarise(
        Total = unique(Total),
        inflation_exp = unique(inflation_exp),
        x = as.numeric(unique(Source)),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        y = ifelse(Type == "Upside", inflation_exp + Total + 0.3, inflation_exp - Total - 0.3),
        label = Type
      )

    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(
        data = stack_data,
        ggplot2::aes(
          xmin = as.numeric(factor(Source)) - 0.3,
          xmax = as.numeric(factor(Source)) + 0.3,
          ymin = ymin,
          ymax = ymax,
          fill = Risk_clean
        ),
        color = "black"
      ) +
      ggplot2::geom_line(
        data = summary_data,
        ggplot2::aes(x = as.numeric(factor(Source)), y = inflation_exp, group = 1),
        color = "grey", size = 1
      ) +
      ggplot2::geom_point(
        data = inflation_points,
        ggplot2::aes(x = x, y = inflation_exp, shape = "Inflation Expectation"),
        color = "grey", size = 2
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(totals_df, Type == "Upside"),
        ggplot2::aes(x = x, y = y, label = label),
        size = 4, vjust = 0
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(totals_df, Type == "Downside"),
        ggplot2::aes(x = x, y = y, label = label),
        size = 4, vjust = 1.2
      ) +
      ggplot2::scale_x_continuous(
        breaks = 1:length(files_with_labels),
        labels = names(files_with_labels)
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, ceiling(max(stack_data$ymax, na.rm = TRUE)), by = 2)
      ) +
      ggplot2::scale_fill_manual(values = colors_named) +
      ggplot2::scale_shape_manual(
        values = c("Inflation Expectation" = 16),
        name = "",
        labels = c("Inflation Expectation")
      ) +
      ggplot2::labs(
        y = ylab,
        x = xlab,
        fill = "Risk Factor",
        title = title
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(order = 1),
        shape = ggplot2::guide_legend(override.aes = list(color = "grey", size = 3), order = 2)
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        text = ggplot2::element_text(),
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.title = ggplot2::element_blank()
      )

    return(p)
  })
}
