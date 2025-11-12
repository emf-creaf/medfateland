# Default control parameters for watershed processes

Defines default control parameters for watershed processes

## Usage

``` r
default_watershed_control(watershed_model = "tetis")
```

## Arguments

- watershed_model:

  Hydrological model for watershed processes. Only "tetis" or "serghei"
  are accepted.

## Value

A list with the following items:

- `watershed_model`: A string with the watershed model.

- `weather_aggregation_factor [= 1]`: An integer specifying the spatial
  aggregation for interpolated weather.

- `tetis_parameters`: A list of TETIS parameters with the following
  elements:

  - `R_localflow [= 1.0]`: Correction factor for soil hydraulic
    saturated conductivity (local vertical flows).

  - `interflow [= TRUE]`: A boolean flag to include interflow.

  - `R_interflow [= 50.0]`: Correction factor for soil hydraulic
    saturated conductivity (subsurface flow between grid cells).

  - `n_interflow [= 1.0]`: Exponent for the determination of interflow.

  - `baseflow [= TRUE]`: A boolean flag to include baseflow.

  - `R_baseflow [= 5.0]`: Correction factor for bedrock hydraulic
    conductivity (groundwaterflow between grid cells).

  - `n_baseflow [= 1.0]`: Exponent for the determination of baseflow.

  - `free_drainage_outlets [= TRUE]`: A boolean flag to prevent water
    table effects into local soil water balance of outlet/channel cells
    (included for numerical stability).

  - `num_daily_substeps [= 1]`: Number of daily sub-steps for interflow
    and baseflow calculations.

  - `subwatersheds [= FALSE]`: A boolean flag to define watershed
    subunits.

  - `max_overlap [= 0.2]`: Maximum proportion of overlapping cells for
    watershed subunits to be considered independent. Lower values will
    normally produce larger subunits.

  - `rock_max_infiltration [= 10]`: Maximum infiltration rate (mm·day-1)
    for rock cells.

  - `deep_aquifer_loss [= 0]`: Daily loss rate from watershed aquifer
    towards a deeper aquifer not connected to outlets (mm·day-1).

  - `n_manning [= 0.035]`: Manning's roughness coefficient.

- `serghei_parameters`: A list of SERGHEI parameters with the following
  elements:

  - `input_dir [= ""]`: Path to SERGHEI input files.

  - `output_dir [= ""]`: Path to SERGHEI output files.

  - `force_equal_layer_widths [= FALSE]`: A boolean flag to force equal
    layer widths (taken from the first soil element) in all soils.

## See also

[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
default_watershed_control()
#> $watershed_model
#> [1] "tetis"
#> 
#> $weather_aggregation_factor
#> [1] 1
#> 
#> $tetis_parameters
#> $tetis_parameters$R_localflow
#> [1] 1
#> 
#> $tetis_parameters$interflow
#> [1] TRUE
#> 
#> $tetis_parameters$R_interflow
#> [1] 50
#> 
#> $tetis_parameters$n_interflow
#> [1] 1
#> 
#> $tetis_parameters$baseflow
#> [1] TRUE
#> 
#> $tetis_parameters$R_baseflow
#> [1] 5
#> 
#> $tetis_parameters$n_baseflow
#> [1] 1
#> 
#> $tetis_parameters$free_drainage_outlets
#> [1] TRUE
#> 
#> $tetis_parameters$num_daily_substeps
#> [1] 1
#> 
#> $tetis_parameters$subwatersheds
#> [1] FALSE
#> 
#> $tetis_parameters$max_overlap
#> [1] 0.2
#> 
#> $tetis_parameters$rock_max_infiltration
#> [1] 10
#> 
#> $tetis_parameters$deep_aquifer_loss
#> [1] 0
#> 
#> $tetis_parameters$n_manning
#> [1] 0.035
#> 
#> 
#> $serghei_parameters
#> $serghei_parameters$input_dir
#> [1] ""
#> 
#> $serghei_parameters$output_dir
#> [1] ""
#> 
#> $serghei_parameters$force_equal_layer_widths
#> [1] FALSE
#> 
#> 
```
