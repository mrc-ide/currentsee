# Calculate flow transitions between columns

Determines valid transitions between consecutive columns in the data,
excluding transitions involving NA values and computing frequencies.

## Usage

``` r
calculate_flow_transitions(dat)
```

## Arguments

- dat:

  Data frame containing the flow data with columns representing steps

## Value

Data frame with columns Var1, Var2, and Freq representing transitions
