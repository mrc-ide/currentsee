# Process data for downward scaling Sankey diagram

Processes intervention scaling data for downward flows (removing
interventions). Extracts columns for current state and 1-3 intervention
removals, calculates percentages, creates formatted node labels, and
removes columns with all NA values.

## Usage

``` r
nodes_down(dat)
```

## Arguments

- dat:

  Data frame containing intervention scaling data with columns named
  "0", "-1", "-2", "-3" representing current state and progressive
  intervention removals

## Value

List containing two elements:

- down:

  Data frame with processed downward scaling data, NA-only columns
  removed

- down_nodes:

  Data frame with columns 'node' and 'label' containing formatted node
  labels with percentages and line breaks for display
