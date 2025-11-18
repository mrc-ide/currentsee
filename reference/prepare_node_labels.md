# Prepare node labels

Joins node data with user-provided labels and creates display labels.
Falls back to original values when custom labels are not provided.

## Usage

``` r
prepare_node_labels(df_n, node_labels = NULL)
```

## Arguments

- df_n:

  Data frame with node data

- node_labels:

  Data frame with columns node and label (optional)

## Value

Data frame with display_label column added
