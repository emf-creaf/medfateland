medfateland - Mediterranean landscape forest simulation
================

<!-- badges: start -->
[![R-CMD-check](https://github.com/emf-creaf/medfateland/workflows/R-CMD-check/badge.svg)](https://github.com/vegmod/medfateland/actions)
<!-- badges: end -->

## Introduction

The R package **medfateland** has been designed to extend the
capabilities of package **medfate** to the landscape level. It allows
running the stand-level models available in medfate on points and cells
within landscape, including the possibility of parallelization.
Additionally, medfateland allows considering spatial hydrological
processes in forested watersheds. Hence, medfateland can be used as a
tool for eco-hydrological applications.

## Package installation

Since both packages evolve together, installing **medfateland** normally
requires an up-to-date version of **medfate**. The package is not
available from CRAN because it is still under active development. Beta
versions of package **medfateland** can be installed from GitHub as
follows:

``` r
remotes::install_github("emf-creaf/medfateland")
```

## Documentation

- The package installation includes a number of *vignettes* that
  illustrate how to run simulation models in **medfateland**.

- The user is also advised to read *vignettes* included in package
  **medfate**.

- Additional documentation of the package, including a *reference book*,
  can be found at <https://emf-creaf.github.io/medfatebook/index.html>.
