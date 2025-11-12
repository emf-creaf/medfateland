# Computing time estimates

## About this vignette

The aim of this vignette is to provide users with a rough estimation of
computing times for simulation models included in package
**medfateland**.

The results presented here were obtained using **3 months of
simulation** with the **example watershed** on a laptop (16 GiB memory
and 11th Gen Inter Core I5 processor @ 2.40 GHz x 8) with Ubuntu Linux
OS.

## Table of computational times

Computational times were estimated using
[`system.time()`](https://rdrr.io/r/base/system.time.html), are in
**seconds** and are shown by **medfate** package version.

| function | transpirationMode | soilDomains | 4.4.0 | 4.7.0 | 4.8.0 | 4.8.1 | 4.8.2 | 4.8.3 | 4.8.4 | 4.8.5 |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| spwb | Granier | buckets | 21.743 | 13.303 | 7.780 | 7.878 | 8.156 | 5.805 | 6.024 | 8.228 |
| spwb | Granier | single | 28.205 | 21.361 | 13.876 | 14.477 | 13.512 | 11.119 | 11.002 | 20.171 |
| spwb | Granier | dual | 56.278 | 52.047 | 40.410 | 36.956 | 37.777 | 33.687 | 39.278 | 57.635 |
| growth | Granier | buckets | 45.589 | 31.272 | 19.963 | 17.741 | 17.420 | 15.259 | 17.669 | 29.019 |
| growth | Granier | single | 56.588 | 41.292 | 27.064 | 24.189 | 22.957 | 20.823 | 25.616 | 42.198 |
| growth | Granier | dual | 71.757 | 61.228 | 56.788 | 45.290 | 47.656 | 43.666 | 60.693 | 94.553 |
