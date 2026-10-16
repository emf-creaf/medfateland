# Computing time estimates

## About this vignette

The aim of this vignette is to provide users with a rough estimation of
computing times for simulation models included in package
**medfateland**.

The results presented here were obtained using **1 year of simulation**
with the **example watershed** (66 grid cells) on a laptop (32 GiB
memory and Intel® Core™ Ultra 9 185H × 22) with Ubuntu Linux OS 26.04.1
LTS.

Several submodel choices are tested for the bulk soil water movement
(`buckets`, `single` or `dual`), whereas transpiration model is kept to
`Granier` and rhizosphereOverlap is set to `total`.

## Table of computational times

Computational times were estimated using
[`system.time()`](https://rdrr.io/r/base/system.time.html), are in
**seconds** and are shown by **medfate** package version.

| function | transpirationMode | rhizosphereOverlap | soilDomains |  5.2.0 |
|:---------|:------------------|:-------------------|:------------|-------:|
| spwb     | Granier           | total              | buckets     |  3.953 |
| spwb     | Granier           | total              | single      |  9.950 |
| spwb     | Granier           | total              | dual        | 35.424 |
| growth   | Granier           | total              | buckets     |  5.444 |
| growth   | Granier           | total              | single      | 12.477 |
| growth   | Granier           | total              | dual        | 38.401 |
