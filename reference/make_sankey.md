# Create bidirectional Sankey diagram

Generates a Sankey diagram that can flow both left (down/removing) and
right (up/adding) from a central current state, with
direction-appropriate flow labels and alignment.

## Usage

``` r
make_sankey(
  x,
  flow_labels = NULL,
  colours = NULL,
  node_width = 0.05,
  node_label_font_size = 3,
  flow_label_font_size = 2.5,
  x_label_font_size = 12,
  x_names = NULL,
  flow_alpha = 0.3,
  gradient_res = 3000
)
```

## Arguments

- x:

  Data frame where each column represents a step and each row represents
  a flow path

- flow_labels:

  Data frame with columns 'flow_start', 'flow_end', 'flow_label' for
  custom flow labels (optional)

- colours:

  Data frame with columns 'node' and 'colour' for custom node colours,
  or named vector (optional)

- node_width:

  Numeric. Width of node rectangles (default: 0.05)

- node_label_font_size:

  Numeric. Font size for node labels (default: 3)

- flow_label_font_size:

  Numeric. Font size for flow labels (default: 2.5)

- x_label_font_size:

  Numeric. Font size for x-axis tick labels

- x_names:

  Character vector. Labels for x-axis (column names by default)

- flow_alpha:

  Numeric. Transparency of flow ribbons (default: 0.3)

- gradient_res:

  Integer. Resolution of flow gradients for smoothness (default: 3000)

## Value

ggplot2 object representing the Sankey diagram
