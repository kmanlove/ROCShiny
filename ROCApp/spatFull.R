spatFull <- function(locations.in, 
                     herd.name,
                     quantile.cut){
  
  locations.orig <- subset(locations.in, Herd == herd.name)
  
  latlong = "+init=epsg:4326 +units=m"
  
  locations <- subset(locations.orig, select = c("Latitude", "Longitude", "SheepID"))
  locations$SheepID <- factor(locations$SheepID)
  
  locations$Latitude <- as.numeric(as.character(locations$Latitude))
  locations$Longitude <- as.numeric(as.character(locations$Longitude))
  
  # make locations into a spatial points data frame
  coordinates(locations) <- ~ Longitude + Latitude 
  
  # specify projection. Note: Lat/long is NOT PROJECTED
  # so the initial spdf has no units.
  proj4string(locations) <- CRS(latlong)
  
  # Reproject to NAD83 for NV (adds units in meters)
  locations <- spTransform(locations, CRS("+init=epsg:2820"))
  
  # Calculate kde using specs from O'Brien et al. 2014 WildSocBull
  kernel.out.list <- kernelUD(locations, h = "href",
                              kern = "bivnorm",
                              grid = 450, same4all = T)
  # get hr volumes and areas (in km2)
  volume.list <- getvolumeUD(kernel.out.list)
  kernel.area.list <- kernel.area(kernel.out.list, 
                                  unin = "m", unout = "km2")
  
  spatpixels.list <- estUDm2spixdf(kernel.out.list)
  
  spatpixels.list$aggregated <- apply(as.matrix(spatpixels.list@data), 1, sum)

  # build an indicator for a potential contour.
  spatpixels.list$ind1 <- ifelse(spatpixels.list$aggregated > quantile(spatpixels.list$aggregated, c(quantile.cut)), 1, 0)
  
  # test the contour -- does it distinguish roughly 95% of the volume?
  in.sum <- sum(spatpixels.list$aggregated[spatpixels.list$ind1 == 1])
  out.sum <- sum(spatpixels.list$aggregated[spatpixels.list$ind1 == 0])
  chhr.ud <- in.sum/(in.sum + out.sum)
  
  # make spatial polys from the spatial pixels indicator field
  levels(spatpixels.list@data$ind1) <- c("include", "omit")
  chhr.raster <- raster(spatpixels.list, layer = dim(spatpixels.list@data)[2])
  chhr.poly <- rasterToPolygons(chhr.raster, fun = function(x){
    x == 1}, dissolve = T)
  
  
  # build indicator for locations out of chhr.polygon
  locations$chhr.ind <-  as.vector(unlist(over(locations, chhr.poly, returnList = F)))
  locations$chhr.ind <- ifelse(is.na(locations$chhr.ind) == T, 0, locations$chhr.ind)
  
  # put dates back into locations data
  locations$Date_ <- as.character(locations.orig$Date_)
  
  # tabulate number of locations out by sheep
  #  table(locations$SheepID, locations$chhr.ind)
  # get sequence of locations out 
  # -- how many strings of consecutive locations?
  # 1) identify locations outside chhr
  indices.out <- which(locations$chhr.ind == 0)
  # 2) break out-locations into sets of consecutive points
  out.strings <- split(indices.out, cumsum(c(1, diff(indices.out) != 1)))
  # 3) length (out.strings) == number forays
  length(out.strings)
  # 4) assign foray number to each string
  foray.number <- vector("list", length(out.strings))
  for(i in 1:length(out.strings)){
    foray.number[[i]] <- rep(i, length(out.strings[[i]]))
  }
  foray.dat <- cbind(do.call("c", out.strings),
                     do.call("c", foray.number))
  # 5) add foray number field to k
  locations$forayNumber <- rep(NA, dim(locations)[1])
  locations$forayNumber[foray.dat[ ,1]] <- foray.dat[ ,2]
  
  
  #------------------------------#
  #-- Foray probabilities -------#
  #------------------------------#
  # O'Brien et al. 2014 then get annual probabilities than an individual (by sex) 
  # would make a foray in May-Oct, or Nov-Apr
  
  # need number of forays per animal, divided by number of years animal was collared
  locations.orig$Date2 <- as.character(locations.orig$Date_)
  locations.orig$Date2 <- as.POSIXlt(strptime(locations.orig$Date2, format = "%m/%d/%y"))
  herd <- id <- sex <- subspp <- start <- stop <- duration.yr <- foray.number <- individ.hr.area.km2 <- rep(NA, length(levels(factor(locations.orig$SheepID))))
  for(i in 1:length(start)){
    individ <- subset(locations.orig, SheepID == levels(factor(locations.orig$SheepID))[i])
    start[i] <- as.character(min(individ$Date2))
    stop[i] <- as.character(max(individ$Date2))
    duration.yr[i] <- difftime(max(individ$Date2), min(individ$Date2))/365
    id[i] <- as.character(individ$SheepID[1])
    sex[i] <- as.character(individ$Sex[1])
    subspp[i] <- as.character(individ$Species[1])
    herd[i] <- as.character(individ$Herd[1])
    individ.hr.area.km2[i] <- kernel.area.list[16, i]
    
    j <- subset(locations, SheepID == levels(factor(locations.orig$SheepID))[i])
    foray.number[i] <- length(unique(j$forayNumber))
  }
  
  forays.per.yr <- foray.number / duration.yr 
  
  individ.data <- as.data.frame(cbind(id, sex, subspp,
                                      herd,
                                      start,
                                      stop,
                                      duration.yr,
                                      foray.number,
                                      forays.per.yr,
                                      individ.hr.area.km2))
  
  names(individ.data) <- c("ID", "Sex", "Subspp", "Herd", "Start", "Stop", "DurationYr",
                           "ForayNumber", "ForaysPerYr", "HrAreaKm2")
  
  
  #-----------------------------#
  #-- Foray distances ----------#
  #-----------------------------#
  
  # 1) extact points corresponding to forays. 
  foray.locs <- subset(locations, is.na(forayNumber) == F)
  
  # LOOP USING gDistance, DISTANCES STORED IN LIST OBJECT
  Fdist <- list()
  for(i in 1:dim(foray.locs)[1]) {
    pDist <- vector()
    for(j in 1:dim(chhr.poly)[1]) { 
      pDist <- append(pDist, gDistance(foray.locs[i,], chhr.poly[j,])) 
    }
    Fdist[[i]] <- pDist
  } 
  
  # return start date and start season for each foray
  foray.locs$Date2 <- as.character(foray.locs$Date_)
  foray.locs$Date2 <- as.POSIXlt(strptime(foray.locs$Date2, format = "%m/%d/%y"))
  
  foray.start <- foray.season <- foray.duration.days <- rep(NA, length(levels(factor(locations$forayNumber))))
  for(i in 1:length(foray.start)){
    specific.foray <- subset(foray.locs, forayNumber == i)
    foray.start[i] <- as.character(specific.foray$Date2[1])
    foray.season[i] <- ifelse(strptime(as.character(specific.foray$Date_[1]), format = "%m/%d/%y")$mon %in% c(4:9), "summer", "winter")
    foray.duration.days[i] <- difftime(max(specific.foray$Date2), min(specific.foray$Date2))
  }
  
  
  # RETURN POLYGON (NUMBER) WITH THE SMALLEST DISTANCE FOR EACH POINT  
  ( min.dist <- unlist(lapply(Fdist, FUN=function(x) which(x == min(x))[1])) ) 
  
  # RETURN DISTANCE TO NEAREST POLYGON
  foray.locs$distanceToChhr <- unlist(lapply(Fdist, FUN=function(x) min(x)[1]))
  
  # get maximum distance for each foray (note: distances in METERS due to projection of foray.locs)
  
  max.dist <- tapply(foray.locs$distanceToChhr, 
                     factor(foray.locs$forayNumber),
                     max)
  #  dev.off()
  #  hist(max.dist, col = "grey80")
  
  # Prop crossing sequential-kilometer bands
  foray.1km <- table(max.dist >= 1000)["TRUE"]/length(max.dist)
  foray.2km <- table(max.dist >= 2000)["TRUE"]/length(max.dist)
  foray.3km <- table(max.dist >= 3000)["TRUE"]/length(max.dist)
  foray.4km <- table(max.dist >= 4000)["TRUE"]/length(max.dist)
  foray.5km <- table(max.dist >= 5000)["TRUE"]/length(max.dist)
  foray.6km <- table(max.dist >= 6000)["TRUE"]/length(max.dist)
  foray.7km <- table(max.dist >= 7000)["TRUE"]/length(max.dist)
  foray.8km <- table(max.dist >= 8000)["TRUE"]/length(max.dist)
  
  foray.props <- c(foray.1km,
                   foray.2km,
                   foray.3km,
                   foray.4km,
                   foray.5km,
                   foray.6km,
                   foray.7km,
                   foray.8km)
  print("Foray proportions beyond different Kms:")
  foray.props
  
  # Build foray data
  id <- sex <- subspp <- herd <- dist <- rep(NA, length(levels(factor(locations@data$forayNumber))))
  
  for(i in 1:length(id)){
    specific.foray <- subset(locations, forayNumber == i)
    id[i] <- as.character(specific.foray$SheepID[1])
    print(id[i])
    sheep <- subset(locations.orig, as.character(SheepID) == as.character(id[i]))
    print(paste("dimension of sheep is", dim(sheep)))
    sex[i] <- as.character(sheep$Sex[1])
    herd[i] <- as.character(sheep$Herd[1])
    subspp[i] <- as.character(sheep$Species[1])
    dist[i] <- max.dist[i]
  }
  
  foray.dat <- as.data.frame(cbind(id, 
                                   sex,
                                   herd,
                                   subspp,
                                   dist,
                                   foray.start,
                                   foray.season,
                                   foray.duration.days))
  
  names(foray.dat) <- c("ID", "Sex", "Herd", "Subspp", "ForayDist", "StartDate", "Season", "Duration.days")
  
  #  boxplot(as.numeric(as.character(foray.dat$ForayDist)) ~ factor(foray.dat$Sex), 
  #          col = "grey80", las = 1)
  
  return(list(chhr.ud = chhr.ud,
              individ.dat = individ.data,
              foray.dat = foray.dat,
              chhr.poly = chhr.poly))
}