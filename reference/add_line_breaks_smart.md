# Add smart line breaks to text strings

Intelligently wraps text strings by adding line breaks at appropriate
positions. Can break at word boundaries (default) or force
character-based breaking for long words. Handles NA values and strings
shorter than the specified width gracefully.

## Usage

``` r
add_line_breaks_smart(x, width = 80, break_words = FALSE)
```

## Arguments

- x:

  Character vector. Text strings to wrap with line breaks

- width:

  Integer. Maximum line width in characters (default: 80)

- break_words:

  Logical. If TRUE, breaks long words at character boundaries. If FALSE
  (default), attempts to break at word boundaries and only forces
  character breaks for words longer than width

## Value

Character vector of the same length as input with line breaks added
using "\n" characters. NA values are returned unchanged.
