# Forest and soil summaries over space

Functions to calculates a summary function for the forest or soil of all
spatial elements in an object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) containing
landscape information.

## Usage

``` r
landscape_summary(
  object,
  name,
  summary_function,
  ...,
  unlist = FALSE,
  progress = FALSE
)
```

## Arguments

- object:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html).

- name:

  A string of the element to summarize: "forest", "soil" or "state".

- summary_function:

  A function that accepts objects of class
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html),
  [`soil`](https://emf-creaf.github.io/medfate/reference/soil.html) or
  model input objects, respectively.

- ...:

  Additional arguments to the summary function.

- unlist:

  Logical flag to try converting the summaries into different columns

- progress:

  Boolean flag to display progress information

## Value

An object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) containing the
calculated statistics. If `unlist = FALSE` column 'summary' is a list
with summaries for each element. If `unlist = TRUE` different columns
are returned instead, one per variable given in the summary function.

## See also

[`forest`](https://emf-creaf.github.io/medfate/reference/forest.html),
[`soil`](https://emf-creaf.github.io/medfate/reference/soil.html),
[`summary.forest`](https://emf-creaf.github.io/medfate/reference/summary.forest.html)

## Author

Miquel De Cáceres Ainsa, CREAF.

## Examples

``` r
# Load plot data and species parameters from medfate
data(example_ifn)

# Load default medfate parameters
data("SpParamsMED")
 
# Apply forest summary function
landscape_summary(example_ifn, "forest", summary.forest, SpParamsMED)
#> Simple feature collection with 100 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1.817095 ymin: 41.93301 xmax: 2.142956 ymax: 41.99881
#> Geodetic CRS:  WGS 84
#> # A tibble: 100 × 2
#>               geometry summary        
#>            <POINT [°]> <list>         
#>  1 (2.130641 41.99872) <smmry.fr [26]>
#>  2 (2.142714 41.99881) <smmry.fr [26]>
#>  3 (1.828998 41.98704) <smmry.fr [26]>
#>  4 (1.841068 41.98716) <smmry.fr [26]>
#>  5 (1.853138 41.98728) <smmry.fr [26]>
#>  6 (1.901418 41.98775) <smmry.fr [26]>
#>  7 (1.937629 41.98809) <smmry.fr [26]>
#>  8  (1.949699 41.9882) <smmry.fr [26]>
#>  9  (1.96177 41.98831) <smmry.fr [26]>
#> 10  (1.97384 41.98842) <smmry.fr [26]>
#> # ℹ 90 more rows
 
```
