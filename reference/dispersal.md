# Seed production, dispersal and seed bank dynamics

Simulates seed bank mortality, seed production and dispersal among
stands

## Usage

``` r
dispersal(
  sf,
  SpParams,
  local_control = medfate::defaultControl(),
  distance_step = 25,
  maximum_dispersal_distance = 3000,
  min_percent = 1,
  stochastic_resampling = FALSE,
  progress = TRUE
)
```

## Arguments

- sf:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) using a UTM
  projection (to measure distances in m) and with the following columns:

  - `geometry`: Spatial geometry.

  - `forest`: Objects of class
    [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html).

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).

- local_control:

  A list of control parameters (see
  [`defaultControl`](https://emf-creaf.github.io/medfate/reference/defaultControl.html))

- distance_step:

  Distance step in meters.

- maximum_dispersal_distance:

  Maximum dispersal distance in meters.

- min_percent:

  A minimum percent of seed bank to retain entry in `seedBank` element
  of `forest`.

- stochastic_resampling:

  A flag to indicate that stochastic resampling of stands is performed.

- progress:

  Boolean flag to display progress information.

## Value

A list with forest objects (for wildland cover type) containing a
modified seed bank

## Details

The input 'sf' object has to be in a Universal Transverse Mercator (UTM)
coordinate system (or any other projection using meters as length unit)
for appropriate function behavior.

Dispersal kernel follows Clark et al. (1999) and potential seed donors
(neighbors) are defined up to a given grid distance order. A maximum
value of 100% seed bank refill is attained for species with seed
production in all seed donors and the local cell.

## References

Clark et al. (1999). Seed dispersal near and far: Patterns across
temperate and tropical forests. Ecology 80(5):1475-1494

## See also

[`fordyn_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)

## Author

Miquel De Cáceres Ainsa, CREAF.

Roberto Molowny-Horas, CREAF.

## Examples

``` r
# \donttest{
data(example_watershed)
data(SpParamsMED)

# Transform to UTM31
example_watershed_utm31 <- sf::st_transform(example_watershed, crs = 32631)

# Estimate seed production and dispersal over the watershed
seedbank_list <- dispersal(example_watershed_utm31, SpParamsMED)
#> ℹ Seed bank mortality
#> ✔ Seed bank mortality [12ms]
#> 
#> ℹ Seed production
#> ✔ Seed production [23ms]
#> 
#> ℹ Kernel estimation
#> ✔ Kernel estimation [1.1s]
#> 
#> ℹ Seed dispersal
#> ✔ Seed dispersal [4.3s]
#> 

seedbank_list[[1]]
#>                   Species   Percent
#> 1          Acer campestre  2.608940
#> 2     Colutea arborescens 12.593774
#> 3     Coriaria myrtifolia  2.161164
#> 4        Cornus sanguinea  1.779891
#> 5        Corylus avellana  1.779891
#> 6          Crataegus spp.  2.523497
#> 7       Cytisus scoparius 11.281491
#> 8             Daphne spp.  2.528290
#> 9          Dorycnium spp. 32.933334
#> 10           Genista spp. 53.627022
#> 11           Hedera helix 17.491668
#> 12     Juniperus communis 11.127593
#> 13         Lavandula spp.  1.863512
#> 14      Ligustrum vulgare 22.116229
#> 15          Lonicera spp. 15.433666
#> 16 Phillyrea angustifolia  5.497512
#> 17       Pinus halepensis  4.348542
#> 18            Pinus nigra 17.521331
#> 19       Pinus sylvestris 28.486686
#> 20     Pistacia lentiscus  5.098280
#> 21           Populus spp.  1.779891
#> 22      Quercus coccifera 34.397097
#> 23        Quercus faginea 21.551717
#> 24           Quercus ilex  6.274062
#> 25           Rhamnus spp.  2.452979
#> 26             Ribes spp. 10.298544
#> 27              Rosa spp. 20.060708
#> 28             Rubus spp. 64.958064
#> 29             Salix spp.  1.779891
#> 30      Salvia rosmarinus 48.519881
#> 31              Ulex spp.  2.434644
#> 32          Viburnum spp. 19.875410

# Transform to UTM31
example_ifn_utm31 <- sf::st_transform(example_ifn, crs = 32631)

# Estimate seed production and dispersal over the set of forest inventory plots
seedbank_list <- dispersal(example_ifn_utm31, SpParamsMED)
#> ℹ Seed bank mortality
#> ✔ Seed bank mortality [17ms]
#> 
#> ℹ Seed production
#> ✔ Seed production [52ms]
#> 
#> ℹ Kernel estimation
#> ✔ Kernel estimation [1.2s]
#> 
#> ℹ Seed dispersal
#> ✔ Seed dispersal [16.8s]
#> 

seedbank_list[[1]]
#>                   Species   Percent
#> 1          Acer campestre  3.930318
#> 2      Buxus sempervirens  1.105246
#> 3     Colutea arborescens 63.099736
#> 4     Coriaria myrtifolia  3.613453
#> 5        Cornus sanguinea  2.423577
#> 6        Corylus avellana  2.423577
#> 7          Crataegus spp.  5.822714
#> 8       Cytisus scoparius 65.683790
#> 9             Daphne spp.  3.930318
#> 10         Dorycnium spp.  2.176420
#> 11           Genista spp. 78.548559
#> 12           Hedera helix 71.033800
#> 13     Juniperus communis 63.856709
#> 14         Lavandula spp.  1.818856
#> 15      Ligustrum vulgare 74.805260
#> 16          Lonicera spp. 64.987150
#> 17 Phillyrea angustifolia  3.687517
#> 18       Pinus halepensis  8.234622
#> 19            Pinus nigra 15.061614
#> 20       Pinus sylvestris 84.642661
#> 21     Pistacia lentiscus  3.101224
#> 22           Populus spp.  2.991203
#> 23      Quercus coccifera 77.907326
#> 24        Quercus faginea 73.178480
#> 25           Quercus ilex  4.735768
#> 26           Rhamnus spp.  4.758933
#> 27             Ribes spp. 64.087529
#> 28              Rosa spp. 73.242211
#> 29             Rubus spp. 90.007868
#> 30             Salix spp.  2.423577
#> 31      Salvia rosmarinus 14.565035
#> 32             Tilia spp.  1.506741
#> 33              Ulex spp.  2.847994
#> 34          Viburnum spp. 76.993421
# }
```
