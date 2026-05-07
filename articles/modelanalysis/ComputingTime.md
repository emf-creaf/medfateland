# Computing time estimates

## About this vignette

The aim of this vignette is to provide users with a rough estimation of
computing times for simulation models included in package
**medfateland**.

The results presented here were obtained using **3 months of
simulation** with the **example watershed** on a laptop (16 GiB memory
and 11th Gen Inter Core I5 processor @ 2.40 GHz x 8) with Ubuntu Linux
OS.

Several submodel choices are tested for the bulk soil water movement
(`buckets`, `single` or `dual`), whereas transpiration model is kept to
`Granier` and rhizosphereOverlap is set to `total`.

## Table of computational times

Computational times were estimated using
[`system.time()`](https://rdrr.io/r/base/system.time.html), are in
**seconds** and are shown by **medfate** package version.

| function | transpirationMode | rhizosphereOverlap | soilDomains | 4.4.0 | 4.7.0 | 4.8.0 | 5.0.0 |
|:---|:---|:---|:---|---:|---:|---:|---:|
| spwb | Granier | total | buckets | 21.743 | 13.303 | 7.780 | 2.279 |
| spwb | Granier | total | single | 28.205 | 21.361 | 13.876 | 4.490 |
| spwb | Granier | total | dual | 56.278 | 52.047 | 40.410 | 24.695 |
| growth | Granier | total | buckets | 45.589 | 31.272 | 19.963 | 3.689 |
| growth | Granier | total | single | 56.588 | 41.292 | 27.064 | 5.796 |
| growth | Granier | total | dual | 71.757 | 61.228 | 56.788 | 26.972 |
