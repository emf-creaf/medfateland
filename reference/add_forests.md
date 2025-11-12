# Add forests

Creates and adds forest data to an
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object by
reading from tree and shrub data tables

## Usage

``` r
add_forests(
  x,
  tree_table = NULL,
  tree_mapping = NULL,
  shrub_table = NULL,
  shrub_mapping = NULL,
  merge_trees = TRUE,
  merge_shrubs = TRUE,
  SpParams = NULL,
  progress = FALSE
)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with a valid
  CRS definition, and a column called 'id'.

- tree_table:

  A data frame with tree records in rows and attributes in columns. Tree
  records can correspond to individual trees or groups of trees with an
  associated density.

- tree_mapping:

  A named character vector to specify mappings of columns in
  `tree_table` into attributes of `treeData`. Accepted names (and the
  corresponding specifications for the columns in `tree_table`) are:

  - "id": Forest stand id.

  - "Species": Species code (should follow codes in `SpParams`).

  - "Species.name": Species name. In this case, the species code will be
    drawn by matching names with species names in `SpParams`.

  - "N": Tree density (in ind./ha).

  - "DBH": Diameter at breast height (in cm).

  - "Height": Tree height (in cm).

  - "plot.size": Plot size (in m2) to which each record refers to. This
    is used to calculate tree density (stems per hectare) when not
    supplied.

  - "Z50": Depth (in mm) corresponding to 50 percent of fine roots.

  - "Z95": Depth (in mm) corresponding to 95 percent of fine roots.

- shrub_table:

  A data frame with shrub records in rows and attributes in columns.
  Records can correspond to individual shrubs (with crown dimensions and
  height) or groups of shrubs with an associated cover estimate.

- shrub_mapping:

  A named character vector to specify mappings of columns in
  `shrub_table` into attributes of `shrubData`. Accepted names (and the
  corresponding specifications for the columns in `shrub_table`) are:

  - "id": Forest stand id.

  - "Species": Species code (should follow codes in `SpParams`).

  - "Species.name": Species name. In this case, the species code will be
    drawn by matching names with species names in `SpParams`.

  - "Cover": Shrub cover (in percent).

  - "D1": Shrub largest crown diameter (in cm).

  - "D2": Shrub crown diameter orthogonal to the largest one (in cm).

  - "Height": Shrub height (in cm).

  - "plot.size": Plot size (in m2) to which each record refers to. This
    is used to calculate shrub cover when shrub data is given at the
    individual level.

  - "Z50": Depth (in mm) corresponding to 50 percent of fine roots.

  - "Z95": Depth (in mm) corresponding to 95 percent of fine roots.

- merge_trees:

  A logical flag to simplify tree cohorts by merging tree records in DBH
  classes (see
  [`forest_mergeTrees`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html)).

- merge_shrubs:

  A logical flag to simplify shrub cohorts by merging shrub records in
  height classes (see
  [`forest_mergeShrubs`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html)).

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html))
  from which valid species names are drawn.

- progress:

  A logical flag to include a progress bar while processing the data.

## Value

A modified object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with column
'forest'.

## Details

The current implementation will replace existing forests of the
indicated 'id' values.

## See also

[`impute_forests()`](https://emf-creaf.github.io/medfateland/reference/forest_parametrization.md),
[`forest_mapWoodyTables`](https://emf-creaf.github.io/medfate/reference/forest_mapWoodyTables.html),
[`forest_mergeTrees`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
# Load tree data
data(poblet_trees)

# Load species parameters
data(SpParamsMED)

# Define sf with three stands
cc <- rbind(c(1.0215, 41.3432),
            c(1.0219, 41.3443), 
            c(1.0219, 41.3443))
d <- data.frame(lon = cc[,1], lat = cc[,2], 
                id = c("POBL_CTL", "POBL_THI_BEF", "POBL_THI_AFT"))
x <- sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
x
#> Simple feature collection with 3 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1.0215 ymin: 41.3432 xmax: 1.0219 ymax: 41.3443
#> Geodetic CRS:  WGS 84
#>             id               geometry
#> 1     POBL_CTL POINT (1.0215 41.3432)
#> 2 POBL_THI_BEF POINT (1.0219 41.3443)
#> 3 POBL_THI_AFT POINT (1.0219 41.3443)

# Define tree mapping 
mapping <- c("id" = "Plot.Code", "Species.name" = "Species", "DBH" = "Diameter.cm")

# Read tree data (warnings are raised)
y_1 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)

# Correct scientific name for downy oak and repeat to avoid losing tree records
poblet_trees$Species[poblet_trees$Species=="Quercus humilis"] <- "Quercus pubescens"
y_1 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)

# Display summary of first forest
summary(y_1$forest[[1]], SpParamsMED)
#> Tree BA (m2/ha): 3.0179815  adult trees: 3.0179815  saplings: 0 
#> Density (ind/ha) adult trees: 267  saplings: 0  shrubs (estimated): 0 
#> Cover (%) adult trees: 42.3272186  saplings: 0  shrubs: 0  herbs: 0 
#> LAI (m2/m2) total: 0.5419688  adult trees: 0.5419688  saplings: 0  shrubs: 0  herbs: 0 
#> Fuel loading (kg/m2) total: 0.1414227  adult trees: 0.1414227  saplings: 0  shrubs: 0  herbs: 0 
#> PAR ground (%): NA  SWR ground (%): NA 

# Add sampled plot surface and repeat reading to correct tree density
poblet_trees$PlotSurface <- 706.86
mapping <- c(mapping, "plot.size" = "PlotSurface")

y_2 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)
summary(y_2$forest[[1]], SpParamsMED)
#> Tree BA (m2/ha): 42.6956049  adult trees: 42.6956049  saplings: 0 
#> Density (ind/ha) adult trees: 3777.2684832  saplings: 0  shrubs (estimated): 0 
#> Cover (%) adult trees: 100  saplings: 0  shrubs: 0  herbs: 0 
#> LAI (m2/m2) total: 4.9189289  adult trees: 4.9189289  saplings: 0  shrubs: 0  herbs: 0 
#> Fuel loading (kg/m2) total: 1.2992685  adult trees: 1.2992685  saplings: 0  shrubs: 0  herbs: 0 
#> PAR ground (%): NA  SWR ground (%): NA 

# Check forests (height is missing!)
check_forests(y_2)
#> ✔ No wildland locations with NULL values in column 'forest'.
#> ✔ All objects in column 'forest' have the right class.
#> ! Missing tree height values detected for 28 (100%) in 3 wildland locations (100%).

# Estimate tree height using general allometric
poblet_trees$Height.cm <- 100 * 1.806*poblet_trees$Diameter.cm^0.518

#Modify mapping to include height and repeat
mapping <- c(mapping, "Height" = "Height.cm")

y_3 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)
summary(y_3$forest[[1]], SpParamsMED)
#> Tree BA (m2/ha): 42.6956049  adult trees: 42.6956049  saplings: 0 
#> Density (ind/ha) adult trees: 3777.2684832  saplings: 0  shrubs (estimated): 0 
#> Cover (%) adult trees: 100  saplings: 0  shrubs: 0  herbs: 0 
#> LAI (m2/m2) total: 4.9189289  adult trees: 4.9189289  saplings: 0  shrubs: 0  herbs: 0 
#> Fuel loading (kg/m2) total: 1.2992685  adult trees: 1.2992685  saplings: 0  shrubs: 0  herbs: 0 
#> PAR ground (%): 6.6842854  SWR ground (%): 13.4793937 

# Final check
check_forests(y_3)
#> ✔ No wildland locations with NULL values in column 'forest'.
#> ✔ All objects in column 'forest' have the right class.
#> ✔ No missing/wrong values detected in key tree/shrub attributes of 'forest' objects.
```
