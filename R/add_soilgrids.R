#' Adds SoilGrids soil description
#'
#' Fills column 'soil' with information drawn from SoilGrids 2.0
#'
#' @param x An object of class \code{\link{sf}} with a valid CRS definition.
#' @param soilgrids_path Path to SoilGrids rasters (see details). If missing, the SoilGrids REST API (https://rest.isric.org) will be queried.
#' @param widths A numeric vector indicating the desired layer widths, in \emph{mm}. If \code{NULL} the default soil grids layer definition is returned.
#' @param replace_existing A logical flag to force the replacement of existing soil data, when already present
#' @param verbose A logical flag to include a progress bar while processing the output of the query to the SoilGrids REST API.
#'
#' @details 
#' 
#' If \code{soilgrids_path = NULL} the function connects with the SoilGrids REST API (https://rest.isric.org)
#' to retrieve the soil physical and chemical characteristics for a site (Hengl \emph{et al}. 2007), selected
#' by its coordinates. Also, in case the depths are not the default ones in the SoilGrids API, the function uses
#' averages the values of soil grid layers depending on the overlap between soil layer definitions. 
#'
#' @return A modified object of class \code{\link{sf}} with column 'soil'.
#'
#' @author \enc{Víctor}{Victor} Granda, EMF-CREAF
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @encoding UTF-8
#' @export
#'
#' @references
#' Hengl T, Mendes de Jesus J, Heuvelink GBM, Ruiperez Gonzalez M, Kilibarda M, \enc{Blagotić}{Blagotic} A, et al. (2017) SoilGrids250m: Global gridded soil information based on machine learning. PLoS ONE 12(2): e0169748. doi:10.1371/journal.pone.0169748.
#'
#' @seealso  \code{\link[medfate]{soil}}, \code{\link[medfate]{defaultSoilParams}}
#'
#' @examples
#'  \dontrun{
#'   # See vignette 'Landscape inputs'
#'  }
#'
add_soilgrids <- function(x, soilgrids_path = NULL, widths = NULL, replace_existing = TRUE, verbose = TRUE) {
  if(!inherits(x, "sf"))  stop("Object 'x' has to be of class 'sf'")
  if(!("land_cover_type" %in% names(x))) stop("'x' should have a colummn called 'land_cover_type'")
  x_lonlat <- sf::st_transform(sf::st_geometry(x), 4326)
  coords <- sf::st_coordinates(x_lonlat)
  
  npoints <- nrow(coords)
  nsoil <- sum(x$land_cover_type %in% c("wildland", "agriculture"))
  
  url.base <- "https://rest.isric.org/soilgrids/v2.0/properties/query?"
  
  
  props_str <- "property=bdod&property=cfvo&property=clay&property=ocd&property=ocs&property=sand&property=silt&property=soc&property=nitrogen"
  depths_str <- "depth=0-5cm&depth=0-30cm&depth=5-15cm&depth=15-30cm&depth=30-60cm&depth=60-100cm&depth=100-200cm"
  
  if(!("soil" %in% names(x))) {
    if(verbose) cli::cli_progress_step("Defining column 'soil'")
    x$soil <- vector("list", npoints)
  }
  if(is.null(soilgrids_path)) {
    if(verbose) {
      cli::cli_progress_step(paste0("Querying ", nsoil," wildland/agriculture points to rest.isric.org:\n"))
      cli::cli_progress_bar(name = "Points", total = npoints)
    }
    for(i in 1:npoints) {
      if(verbose) cli::cli_progress_update()
      if(x$land_cover_type[i] %in% c("wildland", "agriculture")) {
        tryCatch( {
          resSG <- data.frame(matrix(nrow = 6, ncol = 6))
          names(resSG) <- c("widths", "clay", "sand", "om", "bd", "rfc")
          resSG$widths <- c(50,100,150,300,400,1000)
          coord_str <- paste0("lon=",coords[i,1],"&lat=", coords[i,2])
          dest <- paste(coord_str, props_str, depths_str,"value=mean",sep="&")
          url1 <- paste0(url.base, dest)
          path1 <- httr::GET(url1, httr::add_headers("accept"= "application/json"))
          ans.text <- httr::content(path1, as = "text", encoding = "utf-8")
          ans <- jsonlite::fromJSON(ans.text)
          propNames <- ans$properties$layers$name
          d_factors <- ans$properties$layers$unit_measure$d_factor
          for(j in 1:length(propNames)) {
            if(propNames[j]=="clay") {
              resSG$clay = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            } else if(propNames[j]=="sand") {
              resSG$sand = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            } else if(propNames[j]=="bdod") {
              resSG$bd = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            } else if(propNames[j]=="soc") {
              resSG$om = ans$properties$layers$depths[[j]]$values$mean/(d_factors[j]*10)
            } else if(propNames[j]=="nitrogen") {
              resSG$nitrogen = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            } else if(propNames[j]=="cfvo") {
              resSG$rfc = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
            }
          }
          if(is.null(x$soil[[i]]) || replace_existing) {
            if(!is.null(widths)) {
              x$soil[[i]] = medfate::soil_redefineLayers(resSG, widths)
            } else {
              x$soil[[i]] = resSG
            }
          }
        }, error  = function(cond) {
          message(paste("Problems retrieving point",i,": ", cond,"\n"))
        })
      }
    }
    if(verbose) cli::cli_progress_done()
  } else {
    if(verbose) {
      cli::cli_progress_step(paste0("Extracting ", nsoil," wildland/agriculture points from SoilGrids raster layers:\n"))
    }
    vars <- c("sand", "clay", "soc", "nitrogen", "bdod", "cfvo")
    layers <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
    m_var_list <- vector("list", length(vars)) 
    names(m_var_list) <- vars
    for(v in 1:length(vars)) {
      var <- vars[v]
      m_var <- matrix(NA, nrow = nrow(x), ncol = length(layers))
      for(l in 1:length(layers)) {
        layer = layers[l]
        tif_file <- paste0(soilgrids_path, var,"/", var, "_",layer,"_mean.tif")
        r <- terra::rast(tif_file)
        x_vect <- terra::vect(sf::st_transform(sf::st_geometry(x), terra::crs(r)))
        m_var[,l] <- terra::extract(r, x_vect)[,2]
      }
      m_var_list[[v]] <- m_var
    }
    for(i in 1:npoints) {
      if(x$land_cover_type[i] %in% c("wildland", "agriculture")) {
        resSG = data.frame(matrix(nrow = 6, ncol = 6))
        names(resSG) = c("widths", "clay", "sand", "om", "bd", "rfc")
        resSG$widths = c(50,100,150,300,400,1000)
        for(var in vars) {
          if(var=="clay") {
            resSG$clay <- m_var_list[["clay"]][i,]/10
          } else if(var=="sand") {
            resSG$sand <- m_var_list[["sand"]][i,]/10
          } else if(var=="soc") {
            resSG$om <- m_var_list[["soc"]][i,]/100
          } else if(var=="bdod") {
            resSG$bd <- m_var_list[["bdod"]][i,]/100
          } else if(var=="cfvo") {
            resSG$rfc <- m_var_list[["cfvo"]][i,]/10
          } else if(var=="nitrogen") {
            resSG$nitrogen <- m_var_list[["nitrogen"]][i,]/100
          } 
        }
        if(is.null(x$soil[[i]]) || replace_existing) {
          if(!is.null(widths)) {
            x$soil[[i]] = medfate::soil_redefineLayers(resSG, widths)
          } else {
            x$soil[[i]] = resSG
          }
        }
      }
    }
  }
  return(x)
}
