# Apply Filter Selections

Subsets the data based on user filter selections from Shiny inputs.
Filters are only applied when selection is not NULL or "All".

## Usage

``` r
apply_filters(data, filter_vars, input)
```

## Arguments

- data:

  A data frame to be filtered

- filter_vars:

  Character vector of column names to filter on

- input:

  Shiny input object containing filter selections

## Value

A filtered data frame
