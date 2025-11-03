#' Launch the interactive step-package Sankey application
#'
#' Start a Shiny application that allows interactive exploration of simulated
#' step-package Sankey diagrams.
#'
#' @param df Data frame containing at least `id`, `step`, and `package`
#'   columns describing transitions.
#'
#' @return No return value; the function launches a Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   df <- simulate(20)
#'   launch_step_app(df)
#' }
#'
#' @export
launch_step_app <- function(df) {
  opts <- list(
    df = df
  )
  old <- options(currentsee.app = opts)
  on.exit(options(old), add = TRUE)

  app_dir <- system.file("shiny", package = "currentsee")
  shiny::runApp(app_dir, quiet = TRUE)
}
