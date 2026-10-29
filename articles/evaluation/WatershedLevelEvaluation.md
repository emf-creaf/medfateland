# Evaluation of watershed-level runoff against river gauge data

## Introduction

### Goal

The aim of this article is to provide an assessment of the performance
of
[`spwb_land()`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
for the prediction of watershed outflow. To this aim, we simulate
hydrological processes in a set of benchmark watersheds and compare the
model predictions of watershed outflow, after parameter calibration,
against measurements obtained using river gauges at watershed outlets.

### Calibration and simulation

For each watershed, the following procedure has been conducted:

1.  Initial warm-up simulation with default parameters for a specified
    number of years
2.  Calibration of watershed parameters for a specific period (warm-up +
    analysis) using a genetic algorithm and Kling–Gupta efficiency (KGE)
    as optimization function.
3.  Simulation with calibrated watershed parameters.

### Evaluation

For each watershed, simulation results are compared to observed
watershed outflow, differentiating between the period used for
calibration and a posterior period used for independent evaluation. Note
that the performance can decrease for the evaluation period because of
vegetation growth not included in simulations, which may have reduced
observed watershed outflow.

**Goodness-of-fit statistics**

The following goodness of fit statistics are calculated using package
`hydroGOF`:

- Nash-Sutcliffe Efficiency (NSE): This coefficient is sensitive to
  extreme values and might yield sub-optimal results when the dataset
  contains large outliers.
- Kling–Gupta efficiency (KGE): Provides a decomposition of the
  Nash-Sutcliffe efficiency, which facilitates the analysis of the
  importance of different components (bias, correlation and
  variability).
- Index of agreement (d): Initially proposed by Willmott (1981) to
  overcome the drawbacks of the R2, such as the differences in observed
  and predicted means and variances (Legates and McCabe, 1999). d is
  also dimensionless and bounded between 0 and 1 and can be interpreted
  similarly to R2.
- Volumetric efficiency index (VE): Originally proposed by Criss and
  Winston (2008) to circumvent some of the NSE flaws. VE values are also
  bounded \[0, 1\] and represent the fraction of water delivered at the
  proper time.
- Root mean squared error (RMSE): The usual estimation of average model
  error (i.e. the square root of mean squared errors).

**Hydrological analysis**

The density distribution and percentiles of daily discharge are compared
between observations and predictions, for both the calibration and
evaluation periods.

## Watershed (TETIS) parameters

The following table contains the period of calibration and the set of
TETIS parameters employed in simulations on all watersheds after
calibration:

| medfateland | medfate | model | watershed | Calibration_start | Calibration_end | R_localflow | R_interflow | R_baseflow | n_interflow | n_baseflow | num_daily_substeps | rock_max_infiltration | deep_aquifer_loss |
|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 3.0.1 | 5.2.0 | tetis | aiguadora | 2000-08-22 | 2010-08-22 | 0.8730502 | 1.657632 | 0.2147409 | 0.0839125 | 0.5709443 | 24 | 10 | 3.738223 |
| 3.0.1 | 5.2.0 | tetis | siurana | 2000-01-01 | 2009-12-31 | 1.5296833 | 5.497625 | 0.5718424 | 0.0380083 | 0.2362802 | 24 | 10 | 1.159986 |

## Evaluation results

### AIGUADORA watershed with TETIS and version 3.0.1

#### Goodness-of-fit

| Scale   | Period      |    NSE |   KGE |     d |    VE |  RMSE |
|:--------|:------------|-------:|------:|------:|------:|------:|
| Daily   | Calibration |  0.560 | 0.734 | 0.691 | 0.437 | 0.546 |
| Daily   | Evaluation  | -0.542 | 0.256 | 0.414 | 0.023 | 0.840 |
| Monthly | Calibration |  0.655 | 0.733 | 0.731 | 0.562 | 0.311 |
| Monthly | Evaluation  | -0.305 | 0.212 | 0.451 | 0.217 | 0.536 |

#### Time series

*Daily*

![](WatershedLevelEvaluation_files/figure-html/unnamed-chunk-11-1.png)

*Monthly*

![](WatershedLevelEvaluation_files/figure-html/unnamed-chunk-12-1.png)

#### Hydrological analysis (daily scale)

*Density distribution*

![](WatershedLevelEvaluation_files/figure-html/unnamed-chunk-13-1.png)

*Percentiles*

|  | Observed.Calibration | Simulated.Calibration | Observed.Evaluation | Simulated.Evaluation |
|:---|---:|---:|---:|---:|
| 1% | 0.0300 | 0.028 | 0.0600 | 0.027 |
| 5% | 0.0500 | 0.062 | 0.0700 | 0.068 |
| 10% | 0.0600 | 0.104 | 0.0900 | 0.099 |
| 15% | 0.0800 | 0.133 | 0.1000 | 0.138 |
| 25% | 0.1000 | 0.191 | 0.1300 | 0.208 |
| 50% | 0.2600 | 0.412 | 0.2700 | 0.396 |
| 75% | 0.5700 | 0.773 | 0.5400 | 0.845 |
| 85% | 0.9300 | 1.202 | 0.8500 | 1.284 |
| 90% | 1.2400 | 1.487 | 1.1700 | 1.604 |
| 95% | 1.8500 | 2.176 | 1.7660 | 2.387 |
| 99% | 3.7998 | 3.657 | 3.3616 | 4.254 |

#### Computation times

| medfateland_ver | medfate_ver | watershed | model | transpirationMode | soilDomains | rhizosphereOverlap | input | days | Elapsed_time_sec |
|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|
| 3.0.1 | 5.2.0 | aiguadora | spwb | Granier | buckets | total | simplified | 30 | 2.786 |
| 3.0.1 | 5.2.0 | aiguadora | spwb | Granier | single | total | simplified | 30 | 7.882 |
| 3.0.1 | 5.2.0 | aiguadora | spwb | Granier | dual | total | simplified | 30 | 121.689 |
| 3.0.1 | 5.2.0 | aiguadora | spwb | Granier | buckets | total | detailed | 30 | 4.688 |
| 3.0.1 | 5.2.0 | aiguadora | spwb | Granier | single | total | detailed | 30 | 9.842 |
| 3.0.1 | 5.2.0 | aiguadora | spwb | Granier | dual | total | detailed | 30 | 133.196 |
| 3.0.1 | 5.2.0 | aiguadora | growth | Granier | buckets | total | simplified | 30 | 3.634 |
| 3.0.1 | 5.2.0 | aiguadora | growth | Granier | single | total | simplified | 30 | 10.682 |
| 3.0.1 | 5.2.0 | aiguadora | growth | Granier | dual | total | simplified | 30 | 150.666 |
| 3.0.1 | 5.2.0 | aiguadora | growth | Granier | buckets | total | detailed | 30 | 9.430 |
| 3.0.1 | 5.2.0 | aiguadora | growth | Granier | single | total | detailed | 30 | 15.766 |
| 3.0.1 | 5.2.0 | aiguadora | growth | Granier | dual | total | detailed | 30 | 157.929 |

### SIURANA watershed with TETIS and version 3.0.1

#### Goodness-of-fit

| Scale   | Period      |   NSE |   KGE |     d |     VE |  RMSE |
|:--------|:------------|------:|------:|------:|-------:|------:|
| Daily   | Calibration | 0.268 | 0.635 | 0.677 |  0.254 | 0.479 |
| Daily   | Evaluation  | 0.407 | 0.675 | 0.452 | -0.263 | 0.764 |
| Monthly | Calibration | 0.684 | 0.802 | 0.793 |  0.578 | 0.150 |
| Monthly | Evaluation  | 0.109 | 0.321 | 0.455 | -0.052 | 0.300 |

#### Time series

*Daily*

![](WatershedLevelEvaluation_files/figure-html/unnamed-chunk-28-1.png)

*Monthly*

![](WatershedLevelEvaluation_files/figure-html/unnamed-chunk-29-1.png)

#### Hydrological analysis (daily scale)

*Density distribution*

![](WatershedLevelEvaluation_files/figure-html/unnamed-chunk-30-1.png)

*Percentiles*

|  | Observed.Calibration | Simulated.Calibration | Observed.Evaluation | Simulated.Evaluation |
|:---|---:|---:|---:|---:|
| 1% | 0.0000000 | 0.000 | 0.0000000 | 0.012 |
| 5% | 0.0000000 | 0.002 | 0.0000000 | 0.029 |
| 10% | 0.0013747 | 0.003 | 0.0073056 | 0.037 |
| 15% | 0.0116000 | 0.011 | 0.0150000 | 0.044 |
| 25% | 0.0231000 | 0.021 | 0.0250000 | 0.059 |
| 50% | 0.0512000 | 0.054 | 0.0505000 | 0.113 |
| 75% | 0.1249990 | 0.162 | 0.1200006 | 0.281 |
| 85% | 0.2199000 | 0.274 | 0.1924064 | 0.457 |
| 90% | 0.3299600 | 0.402 | 0.2719856 | 0.679 |
| 95% | 0.6722750 | 0.761 | 0.5070718 | 1.225 |
| 99% | 2.0643371 | 2.181 | 1.9439456 | 3.739 |

#### Computation times

| medfateland_ver | medfate_ver | watershed | model | transpirationMode | soilDomains | rhizosphereOverlap | input | days | Elapsed_time_sec |
|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|
| 3.0.1 | 5.2.0 | siurana | spwb | Granier | buckets | total | simplified | 30 | 1.544 |
| 3.0.1 | 5.2.0 | siurana | spwb | Granier | single | total | simplified | 30 | 8.925 |
| 3.0.1 | 5.2.0 | siurana | spwb | Granier | dual | total | simplified | 30 | 205.971 |
| 3.0.1 | 5.2.0 | siurana | spwb | Granier | buckets | total | detailed | 30 | 2.494 |
| 3.0.1 | 5.2.0 | siurana | spwb | Granier | single | total | detailed | 30 | 10.144 |
| 3.0.1 | 5.2.0 | siurana | spwb | Granier | dual | total | detailed | 30 | 206.742 |
| 3.0.1 | 5.2.0 | siurana | growth | Granier | buckets | total | simplified | 30 | 2.028 |
| 3.0.1 | 5.2.0 | siurana | growth | Granier | single | total | simplified | 30 | 7.416 |
| 3.0.1 | 5.2.0 | siurana | growth | Granier | dual | total | simplified | 30 | 233.517 |
| 3.0.1 | 5.2.0 | siurana | growth | Granier | buckets | total | detailed | 30 | 5.485 |
| 3.0.1 | 5.2.0 | siurana | growth | Granier | single | total | detailed | 30 | 11.652 |
| 3.0.1 | 5.2.0 | siurana | growth | Granier | dual | total | detailed | 30 | 225.333 |
