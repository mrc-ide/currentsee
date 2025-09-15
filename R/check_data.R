#' Validate cost-effectiveness data
#'
#' Ensure the required columns are present in the supplied data frame.
#'
#' @param df Data frame containing cost-effectiveness outputs.
#' @param cost_col Name of the cost column.
#' @param effect_col Name of the effect column.
#' @param group_cols Character vector of grouping column names.
#'
#' @return The original `df` invisibly for chaining.
#' @export
check_ce_data <- function(df, cost_col, effect_col, group_cols) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  required <- c(cost_col, effect_col, group_cols)
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
