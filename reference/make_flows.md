# Create flow transitions for bidirectional Sankey diagrams

Processes intervention scaling data to generate flow transitions between
nodes in both upward (adding interventions) and downward (removing
interventions) directions. Calculates flow positions, coordinates, and
label placements while minimizing visual crossings through intelligent
sorting algorithms.

## Usage

``` r
make_flows(x, nodes)
```

## Arguments

- x:

  Data frame containing intervention scaling data where columns
  represent different intervention states (named with numeric values:
  negative for removal steps, 0 for current state, positive for addition
  steps) and rows represent individual flow paths through the
  intervention scaling process

- nodes:

  Data frame containing node information

## Value

Data frame containing detailed flow information with columns:

- `flow_start`, `flow_end`: Character. Source and destination node names

- `direction`: Character. Flow direction ("up" or "down")

- `n`: Integer. Number of individual transitions for this flow

- `*_start`, `*_end`: Numeric. Coordinate boundaries for flow start and
  end points

- `flow_start_center`, `flow_end_center`: Numeric. Center positions of
  flows

- `label_x`, `label_y`: Numeric. Optimal label positioning coordinates

- `hjust`: Numeric. Horizontal text alignment (0 for left, 1 for right)

- `flow_label`: Character. Custom flow descriptions (if available in
  flow_labels)

## Details

The function performs several key operations:

- Identifies upward transitions (positive column progressions)
  representing intervention additions

- Identifies downward transitions (negative column progressions)
  representing intervention removals

- Calculates flow frequencies and positions within source and
  destination nodes

- Applies intelligent sorting to minimize visual flow crossings

- Determines appropriate label positioning and alignment based on flow
  direction

- Joins with external flow_labels data for custom flow descriptions

Flow positioning uses a two-pass sorting algorithm: first sorting flows
within each source node by destination position, then sorting flows
within each destination node by source position. This approach
significantly reduces visual crossing of flow ribbons in the final
Sankey diagram.
