#' Launch the interactive step-package Sankey application
#'
#' Start a Shiny app that allows interactive exploration of step-package
#' Sankey diagrams.
#'
#' @param df Data frame containing step package summaries.
#' @param step_col Name of the step column.
#' @param package_col Name of the package column.
#' @param n_col Name of the count column.
#' @param prop_col Name of the proportion column.
#' @param group_cols Character vector of grouping column names.
#'
#' @return No return value; launches a Shiny application.
#' @export
launch_step_app <- function(df,
                             step_col = "step",
                             package_col = "package",
                             n_col = "n",
                             prop_col = "prop",
                             group_cols = character()) {
  check_step_data(df, step_col, package_col, n_col, prop_col, group_cols)

  opts <- list(
    df = df,
    step_col = step_col,
    package_col = package_col,
    n_col = n_col,
    prop_col = prop_col,
    group_cols = group_cols
  )
  old <- options(currentsee.app = opts)
  on.exit(options(old), add = TRUE)

  app_dir <- system.file("shiny", package = "currentsee")
  shiny::runApp(app_dir, quiet = TRUE)
}
