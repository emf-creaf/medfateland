# Example of watershed

An example of an object of
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with data for a
small catchment of 66 ha (0.66 km2) in Catalonia. Object
`example_watershed_burnin` is the result of three years of burn-in
period.

## Format

The data format is that of an object
[`sf`](https://r-spatial.github.io/sf/reference/sf.html)

## Source

- Watershed limits and channel network from the spanish Ministerio de
  Transición Ecológica y el Reto Demográfico.

- Elevation data at 30 m resolution from catalan Institut Cartogràfic i
  Geològic de Catalunya.

- Soil data from SoilGrids global database (Hengl et al. 2017).

- Soil depth and depth to bedrock from Shangguan et al. (2017).

- Bedrock hydraulic properties from Huscroft et al. (2018).

- Land cover data from Mapa Forestal de España 1:25000.

- Forest structure and composition from Mapa Forestal de España 1:25000
  and the Third Spanish Forest Inventory (IFN3).

## References

Hengl, T., Mendes De Jesus, J., Heuvelink, G.B.M., Gonzalez, M.R.,
Kilibarda, M., Blagotí, A., Shangguan, W., Wright, M.N., Geng, X.,
Bauer-Marschallinger, B., Guevara, M.A., Vargas, R., Macmillan, R.A.,
Batjes, N.H., Leenaars, J.G.B., Ribeiro, E., Wheeler, I., Mantel, S.,
Kempen, B., 2017. SoilGrids250m: Global Gridded Soil Information Based
on Machine Learning. PLoS One 12, e0169748.
doi:10.1371/journal.pone.0169748

Huscroft, J., Gleeson, T., Hartmann, J., Börker, J., 2018. Compiling and
Mapping Global Permeability of the Unconsolidated and Consolidated
Earth: GLobal HYdrogeology MaPS 2.0 (GLHYMPS 2.0). Geophys. Res. Lett.
45, 1897–1904. doi:10.1002/2017GL075860

Shangguan, W., Hengl, T., Mendes de Jesus, J., Yuan, H., Dai, Y., 2017.
Mapping the global depth to bedrock for land surface modeling. J. Adv.
Model. Earth Syst. 9, 65–88. doi:10.1002/2016MS000686

## See also

[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
