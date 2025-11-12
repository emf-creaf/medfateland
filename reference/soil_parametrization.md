# Landscape soil parametrization

Function `add_soilgrids` fills column 'soil' with physical soil
characteristics drawn from SoilGrids 2.0 (Hengl et al. 2017; Poggio et
al. 2021). Function `modify_soils` modifies soil definition according to
soil depth and depth to bedrock information. Function `check_soils`
verifies that soil data does not contain missing values for key
variables and, if so, assigns default values.

## Usage

``` r
add_soilgrids(
  x,
  soilgrids_path = NULL,
  widths = NULL,
  replace_existing = TRUE,
  progress = TRUE
)

modify_soils(
  x,
  soil_depth_map = NULL,
  depth_to_bedrock_map = NULL,
  regolith_rfc = 97.5,
  full_rock_filling = TRUE,
  progress = TRUE
)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with a valid
  CRS definition. If it contains a column called 'land_cover_type',
  soils will be retrieved for "agriculture" and "wildland" cover types
  only. Otherwise, soils are retrieved for all locations. For functions
  `modify_soils` or `check_soils`, `x` should already contain a column
  named "soil".

- soilgrids_path:

  Path to SoilGrids rasters (see details). If missing, the SoilGrids
  REST API (https://rest.isric.org) will be queried.

- widths:

  A numeric vector indicating the desired layer widths, in *mm*. If
  `NULL` the default soil grids layer definition is returned.

- replace_existing:

  A logical flag to force the replacement of existing soil data, when
  already present

- progress:

  A logical flag to include a progress bar while processing the output
  of the query to the SoilGrids REST API.

- soil_depth_map:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  with the soil depth (in *mm*) values.

- depth_to_bedrock_map:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  with depth to bedrock (in *mm*) values.

- regolith_rfc:

  Rock fragment content, in percent volume, between soil depth and 200cm
  depth (or lower depths, if modified via `widths`).

- full_rock_filling:

  Logical flag to modify rock fragment content in all soil layers with
  according to distance to soil depth.

## Value

A modified object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with column
'soil'.

## Details

If `soilgrids_path = NULL` the function connects with the SoilGrids REST
API (https://rest.isric.org) to retrieve the soil physical and chemical
characteristics for a site (Hengl *et al*. 2007; Poggio et al. 2021),
selected by its coordinates. Also, in case the depths are not the
default ones in the SoilGrids API, the function uses averages the values
of soil grid layers depending on the overlap between soil layer
definitions. Unfortunately, SoilGrids REST API queries are limited to a
few points.

If `soilgrids_path != NULL` the function will read SoilGrid rasters from
the file disk. Folders need to be defined for each variable ("sand",
"clay", "soc", "bdod", "cfvo" and "nitrogen"). File paths from
`soilgrids_path` should be named:

*var*/*var*\_*layer*\_mean.tif

where *var* is one of the above and *layer* is "0-5cm", "5-15cm",
"15-30cm", "30-60cm", "60-100cm" or "100-200cm"

SoilGrids does not provide soil depth estimates. Function `modify_soils`
is designed to adjust soil depths according to available information.
When `soil_depth_map` is provided, the function adjusts rock fragment
content of layers below soil depth with the value of `regolith_rfc`.
When `depth_to_bedrock_map` is provided, the function truncates the
total depth of the soil definition to the depth to bedrock. If regional
maps of soil depth are not available, users are recommended to resort on
Shangguan et al (2017).

## References

Hengl T, Mendes de Jesus J, Heuvelink GBM, Ruiperez Gonzalez M,
Kilibarda M, Blagotić A, et al. (2017) SoilGrids250m: Global gridded
soil information based on machine learning. PLoS ONE 12(2): e0169748.
doi:10.1371/journal.pone.0169748.

Poggio L, de Sousa LM, Batjes NH, Heuvelink GBM, Kempen B, Ribeiro E,
Rossiter D (2021). SoilGrids 2.0: producing soil information for the
globe with quantified spatial uncertainty. SOIL 7, 217-240. doi:
10.5194/soil-7-217-2021

Shangguan W, Hengl T, Mendes de Jesus J, Yuan H, Dai J (2017). Mapping
the global depth to bedrock for land surface modeling. Journal of
Advances in Modeling Earth Systems 9: 65-88. doi: 10.1002/2016MS000686

## See also

[`add_topography()`](https://emf-creaf.github.io/medfateland/reference/add_topography.md),
[`impute_forests()`](https://emf-creaf.github.io/medfateland/reference/forest_parametrization.md),
[`soil`](https://emf-creaf.github.io/medfate/reference/soil.html),
[`defaultSoilParams`](https://emf-creaf.github.io/medfate/reference/defaultSoilParams.html)

## Author

Víctor Granda, EMF-CREAF

Miquel De Cáceres Ainsa, EMF-CREAF

## Examples

``` r
 # \donttest{
   library(sf)
   x <- st_sf(geometry = st_sfc(st_point(c(-5.6333, 42.6667)), crs = 4326))
   x_soil <- add_soilgrids(x, widths = c(300, 700, 1000))
#> ℹ Defining new column 'soil'
#> ✔ Defining new column 'soil' [9ms]
#> 
#> ℹ Querying 1 points to rest.isric.org:
#> ✔ Querying 1 points to rest.isric.org: [20ms]
#> 
   x_soil
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -5.6333 ymin: 42.6667 xmax: -5.6333 ymax: 42.6667
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 2
#>            geometry soil        
#>         <POINT [°]> <list>      
#> 1 (-5.6333 42.6667) <df [3 × 7]>
   # See more complete examples in package vignettes 'Preparing inputs'
 # }
```
