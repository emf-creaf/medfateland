# Landscape variables

Extract or estimate variables from landscape objects (class 'sf').

## Usage

``` r
extract_variables(x, vars = "land_cover_type", SpParams = NULL, ...)

plot_variable(x, variable = "land_cover_type", SpParams = NULL, r = NULL, ...)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with the
  appropriate columns.

- vars:

  A string vector with the name of the variables to extract (see
  details).

- SpParams:

  An optional data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)),
  required for some forest stand variables.

- ...:

  Additional arguments (not used).

- variable:

  A string with the name of the variables to draw (see details).

- r:

  An optional object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  defining the raster topology. If supplied, values are shown as raster
  pixels. Otherwise, points are drawn.

## Value

Function `extract_variables()` returns an object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with the
desired variables. Function `plot_variables()` returns a ggplot object.

## Details

The following string values are available for `vars`.

*Topography*:

- `"elevation"`: Elevation in m.

- `"slope"`: Slope in degrees.

- `"aspect"`: Slope in degrees.

- `"land_cover_type"`: Land cover type.

*Snowpack and soil*:

- `"snowpack"`: Snowpack water equivalent (mm). Requires 'snowpack' to
  be defined in `x`.

- `"soil_vol_extract"`: Total water extractable volume (mm).

- `"soil_vol_sat"`: Total water volume at saturation (mm).

- `"soil_vol_fc"`: Total water volume at field capacity (mm).

- `"soil_vol_wp"`: Total water volume at wilting point (mm).

- `"soil_vol_curr"`: Current total water volume (mm).

- `"soil_rwc_curr"`: Current soil relative water content (%).

- `"soil_rew_curr"`: Current soil relative extractable water (%).

- `"soil_theta_curr"`: Current soil moisture content (% vol.)

- `"soil_psi_curr"`: Current soil water potential (MPa).

*Hydrogeology*:

- `"depth_to_bedrock"`: Depth to bedrock (m).

- `"bedrock_porosity"`: Bedrock porosity.

- `"bedrock_conductivity"`: Bedrock conductivity (m/day).

- `"channel"`: River channel network. Requires 'channel' to be defined
  in `x`.

*Aquifer* (requires 'aquifer' to be defined in `x`):

- `"aquifer_elevation"`: Aquifer elevation over bedrock (m).

- `"depth_to_aquifer"`: Depth to aquifer (m).

- `"aquifer"`: Aquifer volume (mm).

*Forest stand*:

- `"basal_area"`: Basal area (m2/ha).

- `"tree_density"`: Tree density (ind/ha).

- `"mean_tree_height"`: Mean tree height (cm).

- `"dominant_tree_height"`: Dominant tree height (cm).

- `"dominant_tree_diameter"`: Dominant tree diameter (cm).

- `"quadratic_mean_tree_diameter"`: Quadratic mean tree diameter (cm).

- `"hart_becking_index"`: Hart-Becking index.

- `"leaf_area_index"`: Leaf area index (m2/m2). Requires `SpParams` to
  be supplied.

- `"foliar_biomass"`: Foliar biomass (kg/m2). Requires `SpParams` to be
  supplied.

- `"fuel_loading"`: Fine live fuel loading (kg/m2). Requires `SpParams`
  to be supplied.

- `"shrub_volume"`: Shrub volume (m3/m2). Requires `SpParams` to be
  supplied.

## See also

[`forest`](https://emf-creaf.github.io/medfate/reference/forest.html),
[`soil`](https://emf-creaf.github.io/medfate/reference/soil.html),
[`summary.forest`](https://emf-creaf.github.io/medfate/reference/summary.forest.html),
[`shinyplot.sf`](https://emf-creaf.github.io/medfateland/reference/shinyplot.sf.md)

## Author

Miquel De Cáceres Ainsa, CREAF.

## Examples

``` r
# Load data and species parameters from medfate
data(example_ifn)
data(SpParamsMED)
  
# Calculate basal area and leaf area index
# for all forest stands
extract_variables(example_ifn, vars = c("basal_area", "leaf_area_index"),
                  SpParams = SpParamsMED)
#> Simple feature collection with 100 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1.817095 ymin: 41.93301 xmax: 2.142956 ymax: 41.99881
#> Geodetic CRS:  WGS 84
#> # A tibble: 100 × 3
#>               geometry basal_area leaf_area_index
#>            <POINT [°]>      <dbl>           <dbl>
#>  1 (2.130641 41.99872)      13.9            5.29 
#>  2 (2.142714 41.99881)      15.7            2.91 
#>  3 (1.828998 41.98704)       0              0.833
#>  4 (1.841068 41.98716)       0              3.03 
#>  5 (1.853138 41.98728)       0              1.79 
#>  6 (1.901418 41.98775)       0              2.76 
#>  7 (1.937629 41.98809)      19.7            5.88 
#>  8  (1.949699 41.9882)       9.91           3.45 
#>  9  (1.96177 41.98831)       0              2.41 
#> 10  (1.97384 41.98842)      19.8            3.75 
#> # ℹ 90 more rows
                  
```
