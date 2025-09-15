#' Launch the interactive Sankey Shiny application
#'
#' Start a Shiny app that allows interactive exploration of cost-effectiveness
#' Sankey diagrams.
#'
#' @param df Data frame containing cost-effectiveness outputs.
#' @param cost_col Name of the cost column.
#' @param effect_col Name of the effect column.
#' @param group_cols Character vector of grouping column names.
#'
#' @return No return value; launches a Shiny application.
#' @export
launch_ce_app <- function(df, cost_col, effect_col, group_cols) {
  check_ce_data(df, cost_col, effect_col, group_cols)

  opts <- list(
    df = df,
    cost_col = cost_col,
    effect_col = effect_col,
    group_cols = group_cols
  )
  old <- options(currentsee.app = opts)
  on.exit(options(old), add = TRUE)

  app_dir <- system.file("shiny", package = "currentsee")
  shiny::runApp(app_dir, quiet = TRUE)
}
