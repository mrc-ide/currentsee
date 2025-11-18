# Simulate package transitions for Sankey examples

Generate stochastic sequences of package adoption to help demonstrate
Sankey diagrams. Each simulation begins from `current` packages and
explores removals and additions until all packages in `int` have been
visited.

## Usage

``` r
simulate(
  n,
  int = c("cm", "itn", "irs", "chem", "vaccine"),
  current = c("cm", "itn"),
  weights = c(itn = 0.6, chem = 0.4, vaccine = 0.2, irs = 0.5)
)
```

## Arguments

- n:

  Number of independent simulations to draw.

- int:

  Character vector giving the ordered set of all possible packages.

- current:

  Character vector of packages present at the baseline step.

- weights:

  Named numeric vector of selection probabilities for packages in `int`.
  Values should lie between 0 and 1.

## Value

A tibble containing one row per step and simulation id.

## Examples

``` r
set.seed(1)
simulate(2)
#> # A tibble: 2 × 7
#>      id `-1`  `0`     `1`              `2`                    `3`        current
#>   <int> <chr> <chr>   <chr>            <chr>                  <chr>      <chr>  
#> 1     1 cm    cm, itn cm, itn, irs     cm, itn, irs, chem     cm, itn, … cm, itn
#> 2     2 cm    cm, itn cm, itn, vaccine cm, itn, chem, vaccine cm, itn, … cm, itn
```
