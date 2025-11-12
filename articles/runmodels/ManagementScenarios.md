# Management scenarios

## Aim

The aim of this vignette is to illustrate how to use **medfateland** (v.
2.8.3) to carry out simulations of forest dynamics on a set of forest
stands while evaluating a demand-based management scenario. In
particular, we will illustrate the use of functions
[`create_management_scenario()`](https://emf-creaf.github.io/medfateland/reference/create_management_scenario.md)
and
[`fordyn_scenario()`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md).

## Preparation

### Forest stands, topography and soils

All simulations in **medfateland** an **sf** object containing
information of the topography, soils and vegetation of a set of target
forest stands. Here we begin by loading the example data set of 100
forest stands distributed on points in the landscape:

``` r
data("example_ifn")
example_ifn
```

    ## Simple feature collection with 100 features and 7 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 1.817095 ymin: 41.93301 xmax: 2.142956 ymax: 41.99881
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 100 × 8
    ##                   geom id        elevation slope aspect land_cover_type soil  
    ##  *         <POINT [°]> <chr>         <dbl> <dbl>  <dbl> <chr>           <list>
    ##  1 (2.130641 41.99872) 081015_A1       680  7.73  281.  wildland        <df>  
    ##  2 (2.142714 41.99881) 081016_A1       736 15.6   212.  wildland        <df>  
    ##  3 (1.828998 41.98704) 081018_A1       532 17.6   291.  wildland        <df>  
    ##  4 (1.841068 41.98716) 081019_A1       581  4.79  174.  wildland        <df>  
    ##  5 (1.853138 41.98728) 081020_A1       613  4.76   36.9 wildland        <df>  
    ##  6 (1.901418 41.98775) 081021_A1       617 10.6   253.  wildland        <df>  
    ##  7 (1.937629 41.98809) 081022_A1       622 20.6   360   wildland        <df>  
    ##  8  (1.949699 41.9882) 081023_A1       687 14.4   324.  wildland        <df>  
    ##  9  (1.96177 41.98831) 081024_A1       597 11.8    16.3 wildland        <df>  
    ## 10  (1.97384 41.98842) 081025_A1       577 14.6   348.  wildland        <df>  
    ## # ℹ 90 more rows
    ## # ℹ 1 more variable: forest <list>

To speed-up simulations in this vignette we select only stands 31 to 40:

``` r
example_subset <- example_ifn[31:40, ]
example_subset
```

    ## Simple feature collection with 10 features and 7 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 1.901727 ymin: 41.96974 xmax: 2.022399 ymax: 41.97083
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 10 × 8
    ##                   geom id        elevation slope aspect land_cover_type soil  
    ##            <POINT [°]> <chr>         <dbl> <dbl>  <dbl> <chr>           <list>
    ##  1 (1.901727 41.96974) 081047_A1       478 12.0   259.  wildland        <df>  
    ##  2 (1.925861 41.96997) 081048_A1       540 16.4   109.  wildland        <df>  
    ##  3 (1.937928 41.97008) 081049_A1       636  7.65  263.  wildland        <df>  
    ##  4 (1.949995 41.97019) 081050_A1       722 18.9   204.  wildland        <df>  
    ##  5  (1.962062 41.9703) 081051_A1       763  9.63  115.  wildland        <df>  
    ##  6 (1.974129 41.97041) 081052_A1       642  9.32  156.  wildland        <df>  
    ##  7 (1.986197 41.97052) 081053_A1       640 17.0   326.  wildland        <df>  
    ##  8 (1.998264 41.97062) 081054_A1       552 10.6    88.7 wildland        <df>  
    ##  9 (2.010331 41.97073) 081055_A1       593  5.73  265.  wildland        <df>  
    ## 10 (2.022399 41.97083) 081056_A1       601  5.26   84.8 wildland        <df>  
    ## # ℹ 1 more variable: forest <list>

### Climate forcing

In this example we will use, for simplicity, the same weather data for
all stands, but normal applications should address spatial variation of
weather. In the following code we prepare a three-year meteorological
data in two blocks (data frames), using the example weather data
provided in **medfate** package:

``` r
data("examplemeteo")
  
meteo_01_02 <- rbind(examplemeteo, examplemeteo)
meteo_01_02$dates <- seq(as.Date("2001-01-01"), 
                         as.Date("2002-12-31"), by="day")
meteo_03 <- examplemeteo
meteo_03$dates <- seq(as.Date("2003-01-01"), 
                      as.Date("2003-12-31"), by="day")
```

### Management prescriptions

Management scenarios require classifying forest stands into *management
units*. Each management unit can be interpreted as a set of stands that
are managed following the same prescriptions. Management units can be
arbitrarily defined, but here we will define them on the basis of
dominant tree species. The following code allows determining the
dominant tree species in each of the 10 forest stands:

``` r
example_subset$dominant_tree_species <- sapply(example_subset$forest,
                                               stand_dominantTreeSpecies, SpParamsMED)
```

And the result is:

``` r
example_subset$dominant_tree_species
```

    ##  [1] "Pinus halepensis" "Pinus nigra"      "Quercus faginea"  "Pinus nigra"     
    ##  [5] "Pinus nigra"      "Pinus halepensis" "Pinus nigra"      "Pinus nigra"     
    ##  [9] "Pinus nigra"      "Pinus nigra"

Package **medfateland** includes a table with default prescription
parameters for a set of species. This is loaded using:

``` r
data("defaultPrescriptionsBySpecies")
```

The columns of this data frame are the same as the parameter names
required by function `defaultManagementFunction()` of package
**medfate**:

``` r
names(defaultPrescriptionsBySpecies)
```

    ##  [1] "Name"                   "SpIndex"                "type"                  
    ##  [4] "targetTreeSpecies"      "thinning"               "thinningMetric"        
    ##  [7] "thinningThreshold"      "thinningPerc"           "minThinningInterval"   
    ## [10] "yearsSinceThinning"     "finalMeanDBH"           "finalPerc"             
    ## [13] "finalPreviousStage"     "finalYearsBetweenCuts"  "finalYearsToCut"       
    ## [16] "plantingSpecies"        "plantingDBH"            "plantingHeight"        
    ## [19] "plantingDensity"        "understoryMaximumCover"

whereas the rows correspond to species or species groups, whose names
are:

``` r
defaultPrescriptionsBySpecies$Name
```

    ##  [1] "Abies/Picea/Pseudotsuga spp."           
    ##  [2] "Betula/Acer spp."                       
    ##  [3] "Castanea sativa"                        
    ##  [4] "Eucalyptus spp."                        
    ##  [5] "Fagus sylvatica"                        
    ##  [6] "Fraxinus spp."                          
    ##  [7] "Juniperus thurifera"                    
    ##  [8] "Cupressus spp."                         
    ##  [9] "Pinus halepensis"                       
    ## [10] "Pinus nigra"                            
    ## [11] "Pinus pinaster"                         
    ## [12] "Pinus pinea"                            
    ## [13] "Pinus radiata"                          
    ## [14] "Pinus sylvestris"                       
    ## [15] "Pinus uncinata"                         
    ## [16] "Chamaecyparis lawsoniana"               
    ## [17] "Thuja spp."                             
    ## [18] "Larix spp."                             
    ## [19] "Quercus ilex"                           
    ## [20] "Quercus faginea"                        
    ## [21] "Quercus suber"                          
    ## [22] "Quercus robur/petraea/rubra/canariensis"
    ## [23] "Quercus pyrenaica/pubescens"            
    ## [24] "Platanus spp."                          
    ## [25] "Populus spp."                           
    ## [26] "Salix spp."                             
    ## [27] "Otras frondosas"

To specify the management unit for stands, we first define a column
`management_unit` with missing values and then assign the corresponding
row number of `defaultPrescriptionsBySpecies` for stands dominated by
each of the three species.

``` r
example_subset$management_unit <- NA
example_subset$management_unit[example_subset$dominant_tree_species=="Pinus halepensis"] <- 9
example_subset$management_unit[example_subset$dominant_tree_species=="Pinus nigra"] <- 10
example_subset$management_unit[example_subset$dominant_tree_species=="Pinus sylvestris"] <- 14
example_subset[,c("id", "dominant_tree_species", "management_unit")]
```

    ## Simple feature collection with 10 features and 3 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 1.901727 ymin: 41.96974 xmax: 2.022399 ymax: 41.97083
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 10 × 4
    ##    id        dominant_tree_species management_unit                geom
    ##    <chr>     <chr>                           <dbl>         <POINT [°]>
    ##  1 081047_A1 Pinus halepensis                    9 (1.901727 41.96974)
    ##  2 081048_A1 Pinus nigra                        10 (1.925861 41.96997)
    ##  3 081049_A1 Quercus faginea                    NA (1.937928 41.97008)
    ##  4 081050_A1 Pinus nigra                        10 (1.949995 41.97019)
    ##  5 081051_A1 Pinus nigra                        10  (1.962062 41.9703)
    ##  6 081052_A1 Pinus halepensis                    9 (1.974129 41.97041)
    ##  7 081053_A1 Pinus nigra                        10 (1.986197 41.97052)
    ##  8 081054_A1 Pinus nigra                        10 (1.998264 41.97062)
    ##  9 081055_A1 Pinus nigra                        10 (2.010331 41.97073)
    ## 10 081056_A1 Pinus nigra                        10 (2.022399 41.97083)

The stands with missing values of `management_unit` (here only one) will
not be managed during simulations.

### Demand-based management scenario

Management scenarios are defined using function
[`create_management_scenario()`](https://emf-creaf.github.io/medfateland/reference/create_management_scenario.md).
Three different kinds of scenarios are allowed. Two of them being
demand-based, meaning that management is constrained by a prescribed
wood demand. The remaining one allows actual management to freely the
interplay of forest dynamics and management prescriptions. Here we will
define a demand-based scenario with fixed demand values for all years of
the simulation. The demand will normally depend on the species available
in the target stands and the area they represent. In this example, we
will require an annual extraction of 2300 m3 of Pinus nigra or P.
sylvestris and 1000 m3 of P. halepensis:

``` r
scen <- create_management_scenario(defaultPrescriptionsBySpecies, 
                                   c("Pinus nigra/Pinus sylvestris" = 1300,
                                     "Pinus halepensis" = 500))
```

Note that we included the data frame `defaultPrescriptionBySpecies` in
our call to
[`create_management_scenario()`](https://emf-creaf.github.io/medfateland/reference/create_management_scenario.md).
This allows having the sylvicultural prescriptions and other management
parameters in a single object. Simulations will assign management
parameters using `management_unit` column and the information in the
prescription data frame.

The scenario object is a list with the following elements.

``` r
names(scen)
```

    ## [1] "scenario_type"            "annual_demand_by_species"
    ## [3] "extraction_rate_by_year"  "units"

The first one specifies the type of scenario, which in this case is
based on a fixed input demand:

``` r
scen$scenario_type
```

    ## [1] "input_demand"

The next element contains the demand values we entered:

``` r
scen$annual_demand_by_species
```

    ## Pinus nigra/Pinus sylvestris             Pinus halepensis 
    ##                         1300                          500

The following element is NULL in this case, since it is used to specify
demand-based scenarios where actual demand depends on observed growth
and a desired rate of extraction:

``` r
scen$extraction_rate_by_year
```

    ## NULL

Finally, element `units` contains the data frame of management units,
which in our case is `defaultPrescriptionBySpecies`.

Normally, we will not need to modify management parameters, but here we
will make thinning operations more likely for all three species by
lowering the basal area threshold that triggers them:

``` r
scen$units[c(9,10,14),"thinningThreshold"] <-20
```

### Area represented by forest stands

Before running simulations, it is necessary to specify the area (in ha)
that each forest stand represents. This is important, because all wood
volumes are defined at the stand level in units of m3/ha. Hence, we need
to multiply these values for the actual area that the stand represents,
in order to know how much of the demand is fulfilled

In this example, we will assume a constant area of 100 ha for all
stands:

``` r
example_subset$represented_area_ha <- 100
```

## Carrying out simulations

We are now ready to launch the simulation of the management scenario.
This is done using a call to function
[`fordyn_scenario()`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md).
As other simulation functions of **medfateland**, we need to supply the
`sf` object, a table of species parameter values and the source of
weather information. We also specify the management scenario and set
`parallelize = TRUE` to speed-up calculations. This last parameter will
also be important in real-case simulations.

``` r
fs_12 <- fordyn_scenario(example_subset, SpParamsMED, meteo = meteo_01_02, 
                         management_scenario = scen,
                         parallelize = TRUE)
```

    ## 

    ## ── Simulation of a management/fire scenario with fordyn ────────────────────────

    ## ℹ Checking sf input

    ## ✔ Checking sf input [19ms]

    ## 

    ## ℹ Checking meteo object input

    ## ✔ Checking meteo object input [36ms]

    ## 

    ## ── Scenario parameters ──

    ## 

    ## • Number of stands: 10

    ## • Represented area: 1000 ha

    ## • Number of years: 2

    ## • Management scenario type: input_demand

    ## • Adding column 'management_arguments'

    ## • Default volume function

    ## • Initial volume: 66449 m3

    ## • Seed dispersal process included.

    ## 

    ## ── Simulation ──

    ## 

    ## ──  [ Year 2001 (1/2) ]

    ## • Demand (incl. offset): 1800 m3

    ## • Determining available volumes and final cuts

    ## • Demand (after final cuts): 1800 m3

    ## • Determining thinning operations

    ## • Seed bank dynamics and seed dispersal...

    ## • Calling fordyn_spatial...

    ## ℹ Checking sf input

    ## ✔ Checking sf input [13ms]

    ## 

    ## ℹ Checking meteo object input

    ## ✔ Checking meteo object input [26ms]

    ## 

    ## ℹ Preparing data for parallelization

    ## ✔ Preparing data for parallelization [24ms]

    ## 

    ## ℹ Launching parallel computation (cores = 7; chunk size = 2)

    ## ✔ Launching parallel computation (cores = 7; chunk size = 2) [29.4s]

    ## 

    ## ℹ Retrieval of results

    ## ✔ Retrieval of results [23ms]

    ## 

    ## ✔ No simulation errors detected

    ## • Final volume: 64078 m3

    ## 

    ## ──  [ Year 2002 (2/2) ]

    ## • Demand (incl. offset): 2763 m3

    ## • Determining available volumes and final cuts

    ## • Demand (after final cuts): 2763 m3

    ## • Determining thinning operations

    ## • Seed bank dynamics and seed dispersal...

    ## • Calling fordyn_spatial...

    ## ℹ Checking sf input

    ## ✔ Checking sf input [17ms]

    ## 

    ## ℹ Checking meteo object input

    ## ✔ Checking meteo object input [20ms]

    ## 

    ## ℹ Preparing data for parallelization

    ## ✔ Preparing data for parallelization [23ms]

    ## 

    ## ℹ Launching parallel computation (cores = 7; chunk size = 2)

    ## ✔ Launching parallel computation (cores = 7; chunk size = 2) [23.5s]

    ## 

    ## ℹ Retrieval of results

    ## ✔ Retrieval of results [19ms]

    ## 

    ## ✔ No simulation errors detected

    ## • Final volume: 62220 m3

    ## 

    ## ── Arranging output ──

    ## 

    ## • Tree/shrub tables

    ## • Wood volume table

Given that it is a long process, the function produces a lot of output
(this can be turned of if `progress = FALSE`). First, the scenario
parameters are presented, including the scenario type, the specified
demand and the number of forest stands in each management unit. Then,
the output is produced for each year, where the function first decides
and reports how many forest stands will be simulated and, after these
are completed, it summarizes the results.

Function
[`fordyn_scenario()`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md)
returns a list whose elements are:

``` r
names(fs_12)
```

    ## [1] "result_sf"             "result_volumes"        "result_volumes_spp"   
    ## [4] "result_volumes_demand" "next_demand"           "next_sf"

The first four elements, those named `result_*` contain the actual
simulation results, whereas the last two elements are used for
subsequent simulations (see next section).

Stand-level results are available in element `result_sf`. The column
names of this `sf` object should be easy to interpret if you have
experience with functions `fordyn()` or
[`fordyn_spatial()`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md):

``` r
fs_12$result_sf
```

    ## Simple feature collection with 10 features and 8 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 1.901727 ymin: 41.96974 xmax: 2.022399 ymax: 41.97083
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 10 × 9
    ##               geometry id        tree_table         shrub_table dead_tree_table
    ##            <POINT [°]> <chr>     <list>             <list>      <list>         
    ##  1 (1.901727 41.96974) 081047_A1 <tibble [50 × 11]> <tibble>    <tibble>       
    ##  2 (1.925861 41.96997) 081048_A1 <tibble [45 × 11]> <tibble>    <tibble>       
    ##  3 (1.937928 41.97008) 081049_A1 <tibble [15 × 11]> <tibble>    <tibble>       
    ##  4 (1.949995 41.97019) 081050_A1 <tibble [47 × 11]> <tibble>    <tibble>       
    ##  5  (1.962062 41.9703) 081051_A1 <tibble [36 × 11]> <tibble>    <tibble>       
    ##  6 (1.974129 41.97041) 081052_A1 <tibble [57 × 11]> <tibble>    <tibble>       
    ##  7 (1.986197 41.97052) 081053_A1 <tibble [62 × 11]> <tibble>    <tibble>       
    ##  8 (1.998264 41.97062) 081054_A1 <tibble [71 × 11]> <tibble>    <tibble>       
    ##  9 (2.010331 41.97073) 081055_A1 <tibble [41 × 11]> <tibble>    <tibble>       
    ## 10 (2.022399 41.97083) 081056_A1 <tibble [64 × 11]> <tibble>    <tibble>       
    ## # ℹ 4 more variables: dead_shrub_table <list>, cut_tree_table <list>,
    ## #   cut_shrub_table <list>, summary <list>

Another important element of the results is `result_volumes`, which
contains several volumes statistics (in m3) summarizing what happened
each year of the simulation.

First, we can inspect for each year the volume corresponding to the
initial and final standing stock, the forest growth and the extracted
wood:

``` r
fs_12$result_volumes[,1:7]
```

    ## # A tibble: 2 × 7
    ##    Year initial growth mortality extracted  final cumulative_growth
    ##   <dbl>   <dbl>  <dbl>     <dbl>     <dbl>  <dbl>             <dbl>
    ## 1  2001  66449.   870.      51.9     3190. 64078.              870.
    ## 2  2002  64078.   786.      48.0     2595. 62220.             1656.

The same figures can be inspected, but corresponding to those species
for which demand has been defined:

``` r
fs_12$result_volumes[,c(1,8:11)]
```

    ## # A tibble: 2 × 5
    ##    Year cumulative_extraction initial_target growth_target mortality_target
    ##   <dbl>                 <dbl>          <dbl>         <dbl>            <dbl>
    ## 1  2001                 3190.         62682.          846.             47.6
    ## 2  2002                 5785.         62643.          770.             46.9

Finally, we can display for each step what was the nominal demand
(according to the input), the actual demand (once the offset of previous
years was included), as well as the cumulative nominal demand and
cumulative extracted volumes. While strong disagreements can exist
between demand and extracted wood at the annual level, the cumulative
columns mentioned are important to check whether simulations fulfilled
the required demand in the long term.

``` r
fs_12$result_volumes[,c(1, 12:16)]
```

    ## # A tibble: 2 × 6
    ##    Year extracted_target final_target nominal_demand demand_offset actual_demand
    ##   <dbl>            <dbl>        <dbl>          <dbl>         <dbl>         <dbl>
    ## 1  2001             837.       64078.           1800            0          1800 
    ## 2  2002            2595.       62220.           1800          963.         2763.

### Continuing a previous simulation

As mentioned above, other elements of the output of
[`fordyn_scenario()`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md)
allow conducting a new scenario simulation starting at the point where
the previous one finished. The element `next_sf` contains the `sf`
object corresponding to the final state of the simulation:

``` r
fs_12$next_sf
```

    ## Simple feature collection with 10 features and 12 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 1.901727 ymin: 41.96974 xmax: 2.022399 ymax: 41.97083
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 10 × 13
    ##                   geom id        elevation slope aspect land_cover_type soil  
    ##  *         <POINT [°]> <chr>         <dbl> <dbl>  <dbl> <chr>           <list>
    ##  1 (1.901727 41.96974) 081047_A1       478 12.0   259.  wildland        <soil>
    ##  2 (1.925861 41.96997) 081048_A1       540 16.4   109.  wildland        <soil>
    ##  3 (1.937928 41.97008) 081049_A1       636  7.65  263.  wildland        <soil>
    ##  4 (1.949995 41.97019) 081050_A1       722 18.9   204.  wildland        <soil>
    ##  5  (1.962062 41.9703) 081051_A1       763  9.63  115.  wildland        <soil>
    ##  6 (1.974129 41.97041) 081052_A1       642  9.32  156.  wildland        <soil>
    ##  7 (1.986197 41.97052) 081053_A1       640 17.0   326.  wildland        <soil>
    ##  8 (1.998264 41.97062) 081054_A1       552 10.6    88.7 wildland        <soil>
    ##  9 (2.010331 41.97073) 081055_A1       593  5.73  265.  wildland        <soil>
    ## 10 (2.022399 41.97083) 081056_A1       601  5.26   84.8 wildland        <soil>
    ## # ℹ 6 more variables: forest <list>, dominant_tree_species <chr>,
    ## #   management_unit <dbl>, represented_area_ha <dbl>,
    ## #   management_arguments <list>, state <list>

On the other hand, in demand-based scenarios there may be demand offsets
that need to be carried on to the next simulations:

``` r
fs_12$next_demand
```

    ## $offset
    ## Pinus nigra/Pinus sylvestris             Pinus halepensis 
    ##                    -29.81761                    197.03491 
    ## 
    ## $last_growth
    ## [1] 769.8502

In addition to the demand offset for each species or species group, note
that `next_demand` also contains information about the last growth. This
is necessary for scenarios where demand is modulated depending on
intended extraction rates and past growth.

If we want to carry on simulations for an extra time period (one year in
our example), we can simply call
[`fordyn_scenario()`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md)
along with the result of the previous simulation instead of the original
`sf` object:

``` r
fs_3 <- fordyn_scenario(fs_12, SpParamsMED, meteo = meteo_03, 
                        management_scenario = scen,
                        parallelize = TRUE)
```

    ## 

    ## ── Simulation of a management/fire scenario with fordyn ────────────────────────

    ## ℹ Recovering previous run

    ## ✔ Recovering previous run [13ms]

    ## 

    ## ℹ Checking sf input

    ## ✔ Checking sf input [25ms]

    ## 

    ## ℹ Checking meteo object input

    ## ✔ Checking meteo object input [19ms]

    ## 

    ## ── Scenario parameters ──

    ## 

    ## • Number of stands: 10

    ## • Represented area: 1000 ha

    ## • Number of years: 1

    ## • Management scenario type: input_demand

    ## • Default volume function

    ## • Initial volume: 62220 m3

    ## • Seed dispersal process included.

    ## 

    ## ── Simulation ──

    ## 

    ## ──  [ Year 2003 (1/1) ]

    ## • Demand (incl. offset): 1967 m3

    ## • Determining available volumes and final cuts

    ## • Demand (after final cuts): 1967 m3

    ## • Determining thinning operations

    ## • Seed bank dynamics and seed dispersal...

    ## • Calling fordyn_spatial...

    ## ℹ Checking sf input

    ## ✔ Checking sf input [14ms]

    ## 

    ## ℹ Checking meteo object input

    ## ✔ Checking meteo object input [21ms]

    ## 

    ## ℹ Preparing data for parallelization

    ## ✔ Preparing data for parallelization [28ms]

    ## 

    ## ℹ Launching parallel computation (cores = 7; chunk size = 2)

    ## ✔ Launching parallel computation (cores = 7; chunk size = 2) [24.3s]

    ## 

    ## ℹ Retrieval of results

    ## ✔ Retrieval of results [20ms]

    ## 

    ## ✔ No simulation errors detected

    ## • Final volume: 62980 m3

    ## 

    ## ── Arranging output ──

    ## 

    ## • Tree/shrub tables

    ## • Wood volume table

Note that in this case, the initial output makes explicit that a
previous simulation is continued.
