#' Summarise intervention packages by step
#'
#' Calculate the proportion of each intervention package within a step
#' for specified grouping variables.
#'
#' @param df Data frame containing prioritisation steps.
#' @param step_col Name of the step column.
#' @param package_col Name of the package column.
#' @param group_cols Character vector of grouping column names.
#'
#' @return A data frame with counts and proportions of packages at each step.
#' @export
summarise_step_packages <- function(df,
                                    step_col = "step",
                                    package_col = "package",
                                    group_cols = character()) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  cols_needed <- c(step_col, package_col, group_cols)
  missing_cols <- setdiff(cols_needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  dplyr::group_by(df, dplyr::across(dplyr::all_of(c(group_cols, step_col, package_col)))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, step_col)))) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup()
}
