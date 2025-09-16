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
                            group_cols = character()
) {
  opts <- list(
    df = df,
    group_cols = group_cols
  )
  old <- options(currentsee.app = opts)
  on.exit(options(old), add = TRUE)

  app_dir <- system.file("shiny", package = "currentsee")
  shiny::runApp(app_dir, quiet = TRUE)
}
