# Parse forestable

Transforms a data frame or sf object issued from package forestables
into an sf object for simulations with medfateland.

## Usage

``` r
parse_forestable(
  x,
  keepSpeciesCodes = TRUE,
  filterMissingSpecies = TRUE,
  filterDeadTrees = TRUE,
  filterCutTrees = TRUE,
  keepUnfilteredCopy = FALSE,
  minimumTreeDBH = 0.1,
  progress = FALSE
)
```

## Arguments

- x:

  A data frame or sf object issued from package forestables.

- keepSpeciesCodes:

  Keeps forest inventory species codes.

- filterMissingSpecies:

  If TRUE, filters out records where species is missing.

- filterDeadTrees:

  If TRUE, filters out dead trees (Spanish forest inventory IFN3 or
  IFN4).

- filterCutTrees:

  If TRUE, filters out cut trees (Spanish forest inventory IFN3 or
  IFN4).

- keepUnfilteredCopy:

  If TRUE, an additional column is given where dead/cut trees have not
  been filtered.

- minimumTreeDBH:

  Minimum DBH for keeping a tree record.

- progress:

  A logical flag to include a progress bar while processing the data.

## Value

An sf object including a 'forest' column. If `keepUnfilteredCopy=TRUE`
an additional column 'forest_unfiltered' is also given.

## Details

This function retrieves the following information from the forestables
object:

- Id unique code, survey year, non-unique plot code and country.

- Plot location. Output geometry is always points in WGS 84. Note that
  exact coordinates are not normally given in forest inventory data.

- Elevation, slope and aspect, whenever available

- Tree and understory data. The function will create a column `forest`
  with this information. If both tree and understory data are missing
  for a given row, the corresponding `forest` will be empty.
