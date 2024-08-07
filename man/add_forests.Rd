% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_forests.R
\name{add_forests}
\alias{add_forests}
\title{Add forests}
\usage{
add_forests(
  x,
  tree_table = NULL,
  tree_mapping = NULL,
  shrub_table = NULL,
  shrub_mapping = NULL,
  merge_trees = TRUE,
  merge_shrubs = TRUE,
  SpParams = NULL,
  progress = FALSE
)
}
\arguments{
\item{x}{An object of class \code{\link[sf]{sf}} with a valid CRS definition, and a column called 'id'.}

\item{tree_table}{A data frame with tree records in rows and attributes in columns. Tree records can correspond to individual trees or groups of trees with an associated density.}

\item{tree_mapping}{A named character vector to specify mappings of columns in \code{tree_table} into attributes of \code{treeData}. Accepted names (and the corresponding specifications for the columns in \code{tree_table}) are:
\itemize{
\item{"id": Forest stand id.}
\item{"Species": Species code (should follow codes in \code{SpParams}).}
\item{"Species.name": Species name. In this case, the species code will be drawn by matching names with species names in \code{SpParams}.}
\item{"N": Tree density (in ind./ha).}
\item{"DBH": Diameter at breast height (in cm).}
\item{"Height": Tree height (in cm).}
\item{"plot.size": Plot size (in m2) to which each record refers to. This is used to calculate tree density (stems per hectare) when not supplied.}
\item{"Z50": Depth (in mm) corresponding to 50 percent of fine roots.}
\item{"Z95": Depth (in mm) corresponding to 95 percent of fine roots.}
}}

\item{shrub_table}{A data frame with shrub records in rows and attributes in columns. Records can correspond to individual shrubs (with crown dimensions and height) or groups of shrubs with an associated cover estimate.}

\item{shrub_mapping}{A named character vector to specify mappings of columns in \code{shrub_table} into attributes of \code{shrubData}. Accepted names (and the corresponding specifications for the columns in \code{shrub_table}) are:
\itemize{
\item{"id": Forest stand id.}
\item{"Species": Species code (should follow codes in \code{SpParams}).}
\item{"Species.name": Species name. In this case, the species code will be drawn by matching names with species names in \code{SpParams}.}
\item{"Cover": Shrub cover (in percent).}
\item{"D1": Shrub largest crown diameter (in cm).}
\item{"D2": Shrub crown diameter orthogonal to the largest one (in cm).}
\item{"Height": Shrub height (in cm).}
\item{"plot.size": Plot size (in m2) to which each record refers to. This is used to calculate shrub cover when shrub data is given at the individual level.}
\item{"Z50": Depth (in mm) corresponding to 50 percent of fine roots.}
\item{"Z95": Depth (in mm) corresponding to 95 percent of fine roots.}
}}

\item{merge_trees}{A logical flag to simplify tree cohorts by merging tree records in DBH classes (see \code{\link[medfate]{forest_mergeTrees}}).}

\item{merge_shrubs}{A logical flag to simplify shrub cohorts by merging shrub records in height classes (see \code{\link[medfate]{forest_mergeShrubs}}).}

\item{SpParams}{A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}) from which valid species names are drawn.}

\item{progress}{A logical flag to include a progress bar while processing the data.}
}
\value{
A modified object of class \code{\link[sf]{sf}} with column 'forest'.
}
\description{
Creates and adds forest data to an \code{\link[sf]{sf}} object by reading from tree and shrub data tables
}
\details{
The current implementation will replace existing forests of the indicated 'id' values.
}
\examples{

# Load tree data
data(poblet_trees)

# Load species parameters
data(SpParamsMED)

# Define sf with three stands
cc <- rbind(c(1.0215, 41.3432),
            c(1.0219, 41.3443), 
            c(1.0219, 41.3443))
d <- data.frame(lon = cc[,1], lat = cc[,2], 
                id = c("POBL_CTL", "POBL_THI_BEF", "POBL_THI_AFT"))
x <- sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
x

# Define tree mapping 
mapping <- c("id" = "Plot.Code", "Species.name" = "Species", "DBH" = "Diameter.cm")

# Read tree data (warnings are raised)
y_1 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)

# Correct scientific name for downy oak and repeat to avoid losing tree records
poblet_trees$Species[poblet_trees$Species=="Quercus humilis"] <- "Quercus pubescens"
y_1 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)

# Display summary of first forest
summary(y_1$forest[[1]], SpParamsMED)

# Add sampled plot surface and repeat reading to correct tree density
poblet_trees$PlotSurface <- 706.86
mapping <- c(mapping, "plot.size" = "PlotSurface")

y_2 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)
summary(y_2$forest[[1]], SpParamsMED)

# Check forests (height is missing!)
check_forests(y_2)

# Estimate tree height using general allometric
poblet_trees$Height.cm <- 100 * 1.806*poblet_trees$Diameter.cm^0.518

#Modify mapping to include height and repeat
mapping <- c(mapping, "Height" = "Height.cm")

y_3 <- add_forests(x, tree_table = poblet_trees, tree_mapping = mapping, SpParams = SpParamsMED)
summary(y_3$forest[[1]], SpParamsMED)

# Final check
check_forests(y_3)

}
\seealso{
\code{\link[=impute_forests]{impute_forests()}}, \code{\link[medfate]{forest_mapWoodyTables}}, \code{\link[medfate]{forest_mergeTrees}}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
}
