# Calculate Plot Width

Calculates dynamic plot width based on the number of non-empty columns
within a specified step range.

## Usage

``` r
calculate_plot_width(data, step_range, width_per_col = 220)
```

## Arguments

- data:

  A data frame containing the plot data

- step_range:

  Numeric vector of step values to include (e.g., -20:20)

- width_per_col:

  Numeric, width in pixels per column (default: 220)

## Value

Numeric value representing total plot width in pixels
