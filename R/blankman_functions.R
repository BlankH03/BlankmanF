#' Run the Friedman Test
#'
#' Conducts the Friedman test for repeated measures data using a tidy data frame.
#'
#' @param data A data frame in long format.
#' @param subject_col Column name for the block/subject identifier.
#' @param treatment_col Column name for the treatment groups.
#' @param response_col Column name for the response variable.
#'
#' @return A list with the test statistic, p-value, degrees of freedom, method, and summary table.
#' @export
run_friedman_test <- function(data, subject_col, treatment_col, response_col) {
  formula <- as.formula(paste(response_col, "~", treatment_col, "|", subject_col))
  result <- stats::friedman.test(formula, data = data)

  summary_table <- data %>%
    dplyr::group_by(.data[[treatment_col]]) %>%
    dplyr::summarise(Mean_Rank = mean(rank(-.data[[response_col]])), .groups = "drop")

  list(
    statistic = result$statistic,
    p_value = result$p.value,
    df = result$parameter,
    method = result$method,
    summary = summary_table
  )
}

#' Plot Mean Ranks from Friedman Test
#'
#' Creates a bar plot of mean treatment ranks from the Friedman test.
#'
#' @param data A tidy data frame.
#' @param subject_col Column name for the subject/block identifier.
#' @param treatment_col Column name for treatment groups.
#' @param response_col Column name for the response variable.
#'
#' @return A ggplot2 object showing the mean ranks.
#' @export
plot_friedman_ranks <- function(data, subject_col, treatment_col, response_col) {
  ranked_data <- data %>%
    dplyr::group_by(.data[[subject_col]]) %>%
    dplyr::mutate(Rank = rank(-.data[[response_col]])) %>%
    dplyr::ungroup()

  summary_ranks <- ranked_data %>%
    dplyr::group_by(.data[[treatment_col]]) %>%
    dplyr::summarise(Mean_Rank = mean(Rank), .groups = "drop")

  ggplot2::ggplot(summary_ranks, ggplot2::aes(x = .data[[treatment_col]], y = Mean_Rank)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Mean Ranks by Treatment", x = "Treatment", y = "Mean Rank") +
    ggplot2::theme_minimal()
}

#' Summarize Friedman Ranks
#'
#' Returns a data frame with mean and standard deviation of ranks per treatment.
#'
#' @param data A tidy data frame.
#' @param subject_col Column name for the subject/block identifier.
#' @param treatment_col Column name for the treatment groups.
#' @param response_col Column name for the response variable.
#'
#' @return A data frame with treatment name, mean rank, and standard deviation of rank.
#' @export
get_friedman_summary <- function(data, subject_col, treatment_col, response_col) {
  ranked_data <- data %>%
    dplyr::group_by(.data[[subject_col]]) %>%
    dplyr::mutate(Rank = rank(-.data[[response_col]])) %>%
    dplyr::ungroup()

  ranked_data %>%
    dplyr::group_by(.data[[treatment_col]]) %>%
    dplyr::summarise(
      Mean_Rank = mean(Rank),
      SD_Rank = sd(Rank),
      .groups = "drop"
    )
}
