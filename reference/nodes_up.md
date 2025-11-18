# Process data for upward scaling Sankey diagram

Processes intervention scaling data for upward flows (adding
interventions). Extracts columns for current state and 1-3 additional
interventions, calculates percentages, creates formatted node labels,
and removes columns with all NA values.

## Usage

``` r
nodes_up(dat)
```

## Arguments

- dat:

  Data frame containing intervention scaling data with columns named
  "0", "1", "2", "3" representing current state and progressive
  intervention additions

## Value

List containing two elements:

- up:

  Data frame with processed upward scaling data, NA-only columns removed

- up_nodes:

  Data frame with columns 'node' and 'label' containing formatted node
  labels with percentages and line breaks for display
