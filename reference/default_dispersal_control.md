# Default control parameters for dispersal

Defines default control parameters for dispersal process

## Usage

``` r
default_dispersal_control()
```

## Value

A list with the following items:

- `distance_step [= 25]`: Distance step in meters.

- `maximum_dispersal_distance [= 3000]`: Maximum dispersal distance in
  meters.

- `min_percent [= 1]`: A minimum percent of seed bank to retain entry in
  `seedBank` element of `forest`.

- `stochastic_resampling [= FALSE]`: A flag to indicate that stochastic
  resampling of stands is performed.

## See also

[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
[`fordyn_scenario`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md)
[`dispersal`](https://emf-creaf.github.io/medfateland/reference/dispersal.md)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
default_dispersal_control()
#> $distance_step
#> [1] 25
#> 
#> $maximum_dispersal_distance
#> [1] 3000
#> 
#> $min_percent
#> [1] 1
#> 
#> $stochastic_resampling
#> [1] FALSE
#> 
```
