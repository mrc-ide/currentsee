# Calculate node positions and dimensions

Computes the position, size, and center coordinates for each node in the
Sankey diagram. Handles NA values by excluding them from calculations
and centers nodes vertically.

## Usage

``` r
make_nodes(x, node_width = 0.05)
```

## Arguments

- x:

  Data frame containing the flow data with columns representing steps

- node_width:

  Numeric. Width of the node rectangles (default: 0.05)

## Value

Data frame with node positions including Var, Freq, xmin, xmax, ymin,
ymax, x_center, y_center
