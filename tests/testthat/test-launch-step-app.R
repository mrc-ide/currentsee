test_that("launch_step_app validates inputs", {
  df <- simulate(2)
  expect_error(launch_step_app(df[, c("step", "package")]), "must contain")
  expect_error(launch_step_app(df, group_cols = "missing"), "not present")
})

test_that("launch_step_app sets options and calls shiny", {
  df <- simulate(1)
  df$season <- "perennial"

  called <- FALSE
  old_opt <- getOption("currentsee.app")

  with_mocked_bindings(
    shiny::runApp = function(appDir, quiet) {
      called <<- TRUE
      expect_true(dir.exists(appDir))
      opts <- getOption("currentsee.app")
      expect_equal(opts$group_cols, "season")
      expect_equal(unique(opts$df$season), "perennial")
      expect_true(quiet)
    },
    {
      launch_step_app(df, group_cols = "season")
    }
  )

  expect_true(called)
  expect_identical(getOption("currentsee.app"), old_opt)
})
