# Launch the interactive step-package Sankey application

Start a Shiny application that allows interactive exploration of
simulated step-package Sankey diagrams.

## Usage

``` r
launch_step_app(df)
```

## Arguments

- df:

  Data frame containing at least `id`, `step`, and `package` columns
  describing transitions.

## Value

No return value; the function launches a Shiny application.

## Examples

``` r
if (interactive()) {
  df <- simulate(20)
  launch_step_app(df)
}
```
