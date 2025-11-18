# Sigmoid interpolation function for smooth flow curves

Creates smooth sigmoid curves for Sankey diagram flows using normal
distribution. This function generates smooth transitions between flow
start and end points.

## Usage

``` r
sankey_sigmoid(x1, x2, y1, y2, n_points = 5000)
```

## Arguments

- x1:

  Numeric. Starting x-coordinate of the flow

- x2:

  Numeric. Ending x-coordinate of the flow

- y1:

  Numeric. Starting y-coordinate of the flow

- y2:

  Numeric. Ending y-coordinate of the flow

- n_points:

  Integer. Number of interpolation points for smoothness (default: 5000)

## Value

Numeric vector of interpolated y-coordinates
