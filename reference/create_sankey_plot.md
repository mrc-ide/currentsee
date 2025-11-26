# Create Sankey Plot

Generates a Sankey diagram with standardized parameters after validating
the input data.

## Usage

``` r
create_sankey_plot(data, step_columns)
```

## Arguments

- data:

  A data frame containing pathway data

- step_columns:

  Character vector of column names representing steps

## Value

A Sankey plot object from currentsee::make_sankey()
