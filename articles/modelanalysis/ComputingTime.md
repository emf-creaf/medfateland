# Computing time estimates

## About this vignette

The aim of this vignette is to provide users with a rough estimation of
computing times for simulation models included in package
**medfateland**.

The results presented here were obtained using **30 days of simulation**
with the **example watershed** (66 grid cells) on a laptop (32 GiB
memory and Intel® Core™ Ultra 9 185H × 22) with Ubuntu Linux OS 26.04.1
LTS.

Both `spwb()` and `growth()` simulation functions are tested.
Combinations of submodel choices are tested for the bulk soil water
movement (`single` or `dual`), the transpiration model (`Granier` or
`Sureau`) and rhizosphereOverlap (`total` or `partial`). Different
inputs are tested, depending on forest complexity: original forest
inventory (most detailed); trees and shrubs aggregated to size classes;
or simplifying the tree and shrub layers to their dominant species.

## Table of computational times

Computational times were estimated using
[`system.time()`](https://rdrr.io/r/base/system.time.html), are in
**seconds** and are shown by **medfate** package version.

| function | transpirationMode | soilDomains | rhizosphereOverlap | nput | days | X5.2.1 |
|:---|:---|:---|:---|:---|---:|---:|
| spwb | Granier | single | total | dominant species | 30 | 0.395 |
| spwb | Granier | dual | total | dominant species | 30 | 0.446 |
| spwb | Granier | single | total | size classes | 30 | 0.460 |
| spwb | Granier | dual | total | size classes | 30 | 0.520 |
| spwb | Granier | single | total | original inventory | 30 | 0.556 |
| spwb | Granier | dual | total | original inventory | 30 | 0.522 |
| spwb | Granier | single | partial | dominant species | 30 | 0.495 |
| spwb | Granier | dual | partial | dominant species | 30 | 0.574 |
| spwb | Granier | single | partial | size classes | 30 | 2.940 |
| spwb | Granier | dual | partial | size classes | 30 | 3.699 |
| spwb | Granier | single | partial | original inventory | 30 | 3.105 |
| spwb | Granier | dual | partial | original inventory | 30 | 3.886 |
| spwb | Sureau | single | total | dominant species | 30 | 1.769 |
| spwb | Sureau | dual | total | dominant species | 30 | 1.822 |
| spwb | Sureau | single | total | size classes | 30 | 13.942 |
| spwb | Sureau | dual | total | size classes | 30 | 13.932 |
| spwb | Sureau | single | total | original inventory | 30 | 13.982 |
| spwb | Sureau | dual | total | original inventory | 30 | 13.934 |
| spwb | Sureau | single | partial | dominant species | 30 | 1.865 |
| spwb | Sureau | dual | partial | dominant species | 30 | 1.959 |
| spwb | Sureau | single | partial | size classes | 30 | 25.191 |
| spwb | Sureau | dual | partial | size classes | 30 | 26.110 |
| spwb | Sureau | single | partial | original inventory | 30 | 25.285 |
| spwb | Sureau | dual | partial | original inventory | 30 | 26.133 |
| growth | Granier | single | total | dominant species | 30 | 0.485 |
| growth | Granier | dual | total | dominant species | 30 | 0.551 |
| growth | Granier | single | total | size classes | 30 | 0.631 |
| growth | Granier | dual | total | size classes | 30 | 0.779 |
| growth | Granier | single | total | original inventory | 30 | 0.717 |
| growth | Granier | dual | total | original inventory | 30 | 0.760 |
| growth | Granier | single | partial | dominant species | 30 | 0.611 |
| growth | Granier | dual | partial | dominant species | 30 | 0.666 |
| growth | Granier | single | partial | size classes | 30 | 3.421 |
| growth | Granier | dual | partial | size classes | 30 | 4.370 |
| growth | Granier | single | partial | original inventory | 30 | 3.654 |
| growth | Granier | dual | partial | original inventory | 30 | 4.236 |
| growth | Sureau | single | total | dominant species | 30 | 4.903 |
| growth | Sureau | dual | total | dominant species | 30 | 4.946 |
| growth | Sureau | single | total | size classes | 30 | 48.862 |
| growth | Sureau | dual | total | size classes | 30 | 49.353 |
| growth | Sureau | single | total | original inventory | 30 | 49.595 |
| growth | Sureau | dual | total | original inventory | 30 | 52.648 |
| growth | Sureau | single | partial | dominant species | 30 | 5.537 |
| growth | Sureau | dual | partial | dominant species | 30 | 5.175 |
| growth | Sureau | single | partial | size classes | 30 | 56.697 |
| growth | Sureau | dual | partial | size classes | 30 | 56.765 |
| growth | Sureau | single | partial | original inventory | 30 | 54.613 |
| growth | Sureau | dual | partial | original inventory | 30 | 58.200 |
