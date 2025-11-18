# Create descriptive x-axis labels for intervention scaling

Converts numeric intervention scaling values to descriptive labels for
Sankey diagram x-axes. Handles current state (0), adding interventions
(positive values), and removing interventions (negative values) with
appropriate formatting.

## Usage

``` r
make_x_labs(x)
```

## Arguments

- x:

  Numeric vector representing intervention scaling steps where:

  - 0 = Current state

  - Positive values = Number of interventions to add

  - Negative values = Number of interventions to remove

## Value

Character vector of formatted labels with line breaks:

- "Current" for value 0

- "Add X\nInterventions" for positive values

- "Remove X\nInterventions" for negative values
