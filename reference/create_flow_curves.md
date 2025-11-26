# Create detailed flow visualization data with node-based color gradients

Generates smooth gradient flows between nodes using the actual node
colors from the nodes data frame for realistic color transitions.

## Usage

``` r
create_flow_curves(flows, nodes, gradient_resolution = 2000)
```

## Arguments

- flows:

  Data frame with flow data including flow_start and flow_end node names

- nodes:

  Data frame with node information including 'node' and 'colour' columns

- gradient_resolution:

  Integer. Number of segments for gradient smoothness (default: 2000)

## Value

Data frame with detailed flow rendering data including color gradients
