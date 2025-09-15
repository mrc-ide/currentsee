#' Validate step-package data
#'
#' Ensure the required columns are present in the supplied data frame of
#' summarised step packages.
#'
#' @param df Data frame containing step package summaries.
#' @param step_col Name of the step column.
#' @param package_col Name of the package column.
#' @param n_col Name of the count column.
#' @param prop_col Name of the proportion column.
#' @param group_cols Character vector of grouping column names.
#'
#' @return The original `df` invisibly for chaining.
#' @export
check_step_data <- function(df,
                            step_col = "step",
                            package_col = "package",
                            n_col = "n",
                            prop_col = "prop",
                            group_cols = character()) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  required <- c(step_col, package_col, n_col, prop_col, group_cols)
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(df)
}
