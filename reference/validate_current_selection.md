# Validate Current Selection

Validates that the 'current' column has exactly one non-NA value that is
not "All", which is required for Sankey plot generation.

## Usage

``` r
validate_current_selection(data)
```

## Arguments

- data:

  A data frame containing a 'current' column

## Value

NULL (function validates or throws error via shiny::validate)
