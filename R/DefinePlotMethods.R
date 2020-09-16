setMethod("spplot", signature("SpatialGridLandscape"),
          function(obj, variable = "lct", ...) {
            if(variable=="lct") {
              spplot(SpatialGridDataFrame(obj@grid, data.frame(lct = obj@lct)),...)
            }
            else if(variable=="basalArea") {
              n = length(obj@soillist)
              ba = rep(NA, n)
              for(i in 1:n) {
                if(!(obj@lct[i] %in% c("Rock","Static"))) ba[i] = forest_basalArea(obj@forestlist[[i]])
              }
              spplot(SpatialGridDataFrame(obj@grid, data.frame(ba = ba)),...)
            }
            else if(variable=="texture1") {
              n = length(obj@soillist)
              usda = rep(NA, n)
              for(i in 1:n) {
                if(!(obj@lct[i] %in% c("Rock","Static"))) usda[i] = obj@soillist[[i]]$usda_Type[1]
              }
              spplot(SpatialGridDataFrame(obj@grid, data.frame(texture1 = usda)),...)
            }
            else if(variable=="texture2") {
              n = length(obj@soillist)
              usda = rep(NA, n)
              for(i in 1:n) {
                if(!(obj@lct[i] %in% c("Rock","Static"))) usda[i] = obj@soillist[[i]]$usda_Type[2]
              }
              spplot(SpatialGridDataFrame(obj@grid, data.frame(texture2 = usda)),...)
            }
            else if(variable=="W1") {
              n = length(obj@soillist)
              W1 = rep(NA, n)
              for(i in 1:n) if(!(obj@lct[i] %in% c("Rock","Static"))) W1[i] = obj@soillist[[i]]$W[1]
              spplot(SpatialGridDataFrame(obj@grid, data.frame(W1 = W1)), at = seq(0,1, by=0.01),...)
            }
            else if(variable=="W2") {
              n = length(obj@soillist)
              W2 = rep(NA, n)
              for(i in 1:n) if(!(obj@lct[i] %in% c("Rock","Static"))) W2[i] = obj@soillist[[i]]$W[2]
              spplot(SpatialGridDataFrame(obj@grid, data.frame(W2 = W2)), at = seq(0,1, by=0.01),...)
            }
            else if(variable=="W3") {
              n = length(obj@soillist)
              W3 = rep(NA, n)
              for(i in 1:n) if(!(obj@lct[i] %in% c("Rock","Static"))) W3[i] = obj@soillist[[i]]$W[3]
              spplot(SpatialGridDataFrame(obj@grid, data.frame(W3 = W3)), at = seq(0,1, by=0.01),...)
            }
            else if(variable=="WTD") {
              n = length(obj@soillist)
              WTD = rep(NA, n)
              for(i in 1:n) if(!(obj@lct[i] %in% c("Rock","Static"))) WTD[i] = medfate::soil_waterTableDepth(obj@soillist[[i]])
              spplot(SpatialGridDataFrame(obj@grid, data.frame(WTD = WTD)), ...)
            } else {
              spplot(as(obj, "SpatialGridTopography"), variable, ...)
            }
          })


setMethod("spplot", signature("SpatialPointsLandscape"),
          function(obj, variable = "basalArea", ...) {
            if(variable=="basalArea") {
              n = length(obj@soillist)
              ba = rep(NA, n)
              for(i in 1:n) ba[i] = forest_basalArea(obj@forestlist[[i]])
              spplot(SpatialPointsDataFrame(obj@coords, data.frame(ba = ba)),...)
            }
            else if(variable=="texture1") {
              n = length(obj@soillist)
              usda = rep(NA, n)
              for(i in 1:n) usda[i] = obj@soillist[[i]]$usda_Type[1]
              spplot(SpatialPointsDataFrame(obj@coords, data.frame(texture1 = usda)),...)
            }
            else if(variable=="texture2") {
              n = length(obj@soillist)
              usda = rep(NA, n)
              for(i in 1:n) usda[i] = obj@soillist[[i]]$usda_Type[2]
              spplot(SpatialPointsDataFrame(obj@coords, data.frame(texture2 = usda)),...)
            }
            else if(variable=="W1") {
              n = length(obj@soillist)
              W1 = rep(NA, n)
              for(i in 1:n) W1[i] = obj@soillist[[i]]$W[1]
              spplot(SpatialPointsDataFrame(obj@coords, data.frame(W1 = W1)), at = seq(0,1, by=0.01),...)
            }
            else if(variable=="W2") {
              n = length(obj@soillist)
              W2 = rep(NA, n)
              for(i in 1:n) W2[i] = obj@soillist[[i]]$W[2]
              spplot(SpatialPointsDataFrame(obj@coords, data.frame(W2 = W2)), at = seq(0,1, by=0.01),...)
            }
            else if(variable=="W3") {
              n = length(obj@soillist)
              W3 = rep(NA, n)
              for(i in 1:n) W3[i] = obj@soillist[[i]]$W[3]
              spplot(SpatialPointsDataFrame(obj@coords, data.frame(W3 = W3)), at = seq(0,1, by=0.01),...)
            }
            else if(variable=="WTD") {
              n = length(obj@soillist)
              WTD = rep(NA, n)
              for(i in 1:n) WTD[i] = medfate::soil_waterTableDepth(obj@soillist[[i]])
              spplot(SpatialPointsDataFrame(obj@coords, data.frame(WTD = WTD)), ...)
            } else {
              spplot(as(obj, "SpatialPointsTopography"), variable, ...)
            }
          })
