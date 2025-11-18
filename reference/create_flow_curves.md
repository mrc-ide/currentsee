# Create detailed flow visualization data with smooth curves

Generates the detailed geometric data needed to render smooth gradient
flows between nodes, including color interpolation and curve
coordinates.

## Usage

``` r
create_flow_curves(df_f_positioned, gradient_resolution = 2000)
```

## Arguments

- df_f_positioned:

  Data frame with positioned flow data including color and coordinate
  information

- gradient_resolution:

  Integer. Number of segments for gradient smoothness (default: 2000)

## Value

Data frame with detailed flow rendering data including color, xmin,
xmax, ymin, ymax
