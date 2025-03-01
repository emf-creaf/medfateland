
# Mediterranean landscape simulation <a href="https://emf-creaf.github.io/medfateland/"><img src="man/figures/logo.png" align="right" height="139" alt="medfateland website" /></a>

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/medfateland)](https://cran.r-project.org/package=medfateland)
[![](https://cranlogs.r-pkg.org/badges/medfateland)](https://cran.rstudio.com/web/packages/medfateland/index.html)
[![R-CMD-check](https://github.com/emf-creaf/medfateland/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emf-creaf/medfateland/actions)

## Introduction

The R package **medfateland** has been designed to extend the
capabilities of package
[**medfate**](https://emf-creaf.github.io/medfate/) to a
spatially-explicit context. It allows running the stand-level models
available in medfate on points and cells within landscape or region,
including the possibility of parallelization. Additionally, medfateland
allows considering seed dispersal and lateral water transfer processes
in forested watersheds. Hence, medfateland can be used as a tool for
eco-hydrological applications.

## Package installation

Since both packages evolve together, installing **medfateland** normally
requires an up-to-date version of
[**medfate**](https://emf-creaf.github.io/medfate/).

Package **medfateland** can be found at
[CRAN](https://CRAN.R-project.org/package=medfateland), where it is
updated every few months. Installation from CRAN can be done via:

``` r
install.packages("medfateland")
```

Users can also download and install the latest stable versions GitHub as
follows (required package `remotes` should be installed/updated first):

``` r
remotes::install_github("emf-creaf/medfateland")
```

## Documentation

A number of *vignettes* illustrate how to initialize inputs and run
simulation models in **medfateland**. These can be found at the package
[website](https://emf-creaf.github.io/medfateland/). The user is also
advised to read the articles included in
[**medfate**](https://emf-creaf.github.io/medfate/).

A complete documentation of both packages, including a *reference book*,
can be found at <https://emf-creaf.github.io/medfatebook/index.html>.

## Companion R packages

The modelling tools included in **medfate** and **medfateland** are
closely developed with another package:

- Package [**traits4models**](https://emf-creaf.github.io/traits4models)
  provides functions to help creating species parameter inputs for
  process-based models such as those in for **medfate** and
  **medfateland** functions.

Two other R packages complete the simulation framework, but can be used
for many other purposes beyond forest modelling:

- Package [**meteoland**](https://emf-creaf.github.io/meteoland) allows
  generating daily weather input for simulation models in **medfate**
  and **medfateland**. Package **meteoland** is a dependency for
  **medfate** and **medfateland**, but can be used independently to
  obtain daily weather data.
- Package [**forestables**](https://emf-creaf.github.io/forestables)
  allows reading and harmonizing forest inventory data to a common data
  structure. Initialization workflows in **medfateland** can use data
  from **forestables**, but the data structures of the package can be
  used for many studies beyond modelling.

The relationships between the five packages are illustrated in the
figure below, where black arrows indicate package dependencies and gray
arrows indicate model parameter or data provision.

<img src="man/figures/packages.png" width="80%" style="display: block; margin: auto;" />

## Authorship

Package **medfateland** is developed and maintained by the [*Ecosystem
Modelling Facility*](https://emf.creaf.cat) unit at
[*CREAF*](https://www.creaf.cat/) (in Spain), in close collaboration
with researchers from
[*URFM-INRAE*](https://www6.paca.inrae.fr/ecologie_des_forets_mediterraneennes/)
(in France) and [*CTFC*](https://www.ctfc.cat/) (in Spain).

<img src="man/figures/institution_logos.png" width="60%" style="display: block; margin: auto;" />

## Funding

- **Research project**: Boosting process-based models to project forest
  dynamics and associated ecosystem services at stand-to-regional scales
  (BOMFORES). **Financial Entity**: Ministerio de Ciencia e Innovación
  (PID2021-126679OB-I00). **Duration from**: 01/09/2022 **to**:
  31/08/2024. **PI**: Miquel De Cáceres.
- **Research project**: Improving the modelling of key forest dynamic
  processes to forecast long-term changes in Mediterranean forests under
  climate change (IMPROMED). **Financial Entity**: Ministerio de Ciencia
  e Innovación (PID2023-152644NB-I00). **Duration from**: 01/09/2024
  **to**: 31/08/2025. **PI**: Miquel De Cáceres/Josep Mª Espelta.
