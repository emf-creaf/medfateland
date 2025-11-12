# Default volume function

Example function for estimating wood volume (in m3/ha) from a tree table
or forest object.

## Usage

``` r
default_volume_function(x, SpParams = NULL)
```

## Arguments

- x:

  A data frame with columns 'DBH', 'Height' and 'N' or a
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
  object

- SpParams:

  A data frame with species parameters (not used in the default function
  but will be called)

## Value

A function amenable for wood volume estimation.

## Details

Users should define their own functions taking into account that:

- Input should be named 'x' and consist of a tree table with tree
  records as rows and columns 'DBH' (cm), 'Height' (cm), and 'N'
  (ind./ha).

- Output should be a numeric vector of length equal to the number of
  tree records in 'x'
