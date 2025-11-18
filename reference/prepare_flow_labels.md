# Prepare flow label positions

Calculates optimal positions for flow labels and joins with
user-provided label data. Positions labels at the start of flows with
proportional spacing.

## Usage

``` r
prepare_flow_labels(df_f_positioned, flow_labels = NULL)
```

## Arguments

- df_f_positioned:

  Data frame with positioned flow data

- flow_labels:

  Data frame with columns flow_start, flow_end, flow_label (optional)

## Value

Data frame with flow label positions and text
