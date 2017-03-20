# clear environment
rm(list=ls())

#===================================================
# Simulate data, in case real data is not available
#===================================================

#  Simulate

# n = 5
# m = 10
# # extent
# box  <- data.frame(x=c(8.3939,8.7427), y=c(47.3018,47.4406))
# box  
# plot(box)  
# 
# # read coordinates of bus stops
# busStops <- data.frame(x=runif(n, min(box$x),  max(box$x)), y=runif(n,  min(box$y),  max(box$y)))
# busStops
# 
# # read coordinates of locations
# locations <- data.frame(x=runif(m,  min(box$x),  max(box$x)), y=runif(m, min(box$y),  max(box$y)))
# locations['type'] <- sample(1:3,nrow(locations), replace=T)
# locations

#===================================================
# Read data
#===================================================
library(RCurl)

# Landmarks with given classes ('type'). We limited ourselves for three of them
x <- getURL("https://raw.githubusercontent.com/OpenDataDayZurich2016/odd-zurich-stops-classification/master/data-examples/overpass_classes.csv")
landmarks <- read.csv(text = x)
names(landmarks) <- c('lat','lon','type')
head(landmarks)

# Bus stops 
x <- getURL("https://raw.githubusercontent.com/OpenDataDayZurich2016/odd-zurich-stops-classification/master/data-examples/haltestellen_coordinates.csv")
busStops <- read.csv(text = x)
names(busStops) <- c('lat','lon')
head(busStops)

#busStops <- busStops[sample(1:nrow(busStops),5),]
#busStops

# We know there are just 3 classes
cols3 <- c('steelblue4','springgreen4','yellow')

cols_locs <- ifelse(landmarks$type==1,cols3[1],ifelse(landmarks$type==2,cols3[2],cols3[3]))

#====================================
# Plot raw data
#====================================
# find extent
box  <- data.frame(lon=c(min(landmarks$lon,busStops$lon),max(landmarks$lon,busStops$lon)), 
                   lat=c(min(landmarks$lat,busStops$lat),max(landmarks$lat,busStops$lat)))
plot(box)
points(busStops$lon,busStops$lat, col=rgb(0,0,0), pch=8, cex=1)
points(landmarks$lon, landmarks$lat, col=cols_locs, pch=16, cex=1)

#====================================
# Compute travel time by walking
#====================================
require("gmapsdistance")

# dist_mat[i,j] - walking time from bus stop i to landmark j
dist_mat <- matrix(data = NA, nrow = nrow(busStops), ncol = nrow(landmarks))

# THIS SHOULD NOT BE DONE MANUALLY
for (i in 1:nrow(busStops)){
  for (j in 1:nrow(landmarks)){
    origin <- paste(busStops$lat[i],busStops$lon[i], sep='+')
    destination <- paste(landmarks$lat[j],landmarks$lon[j], sep='+')
    results = gmapsdistance(origin = origin, destination = destination, mode = "walking")
    results
    dist_mat[i,j] <- results$Time
  }
}

dist_mat_inv <- 1/dist_mat #exp(-dist_mat)

#====================================
# Aggregate inverse times by location type and scale
#====================================
dist_mat_type <- matrix(data = 0, nrow = nrow(busStops), ncol = 3)

for (i in 1:dim(dist_mat)[1]){
  for (type in 1:3){
    dist_mat_type[i,type] <- sum(dist_mat_inv[i, which(landmarks$type==type)])
  }
}

dist_mat_type

# scale1 <- function(x){return(1/x)}
# dmat <- t(apply(dist_mat_type, MARGIN = 1, FUN=scale1))

scale2 <- function(x){return(x/sum(x))}
dmat <- t(apply(dist_mat_type, MARGIN = 1, FUN=scale2))
dmat

#====================================
# Visualize result
#====================================

#====================================
# Attempt 1 : pie charts
#====================================
plot(box)
points(landmarks$lon, landmarks$lat, col=cols_locs, pch=16, cex=1)
require(mapplots)
for (i in 1:dim(dmat)[1]){
  lon <- busStops$lon[i]
  lat <- busStops$lat[i]
  z <- dmat[i,]
  r <- (max(box$lon)-min(box$lon))/60
  # add pie chart 
  add.pie(z=z, x=lon, y=lat, col=cols3, labels=NA, radius=r)
}

#====================================
# Attempt 2 : mixing colrors
#====================================
plot(box)
points(landmarks$lon, landmarks$lat, col=cols_locs, pch=16, cex=1)
require(scales)
points(busStops$lon-0.0001, busStops$lat-0.0001, col=alpha(cols3[1], dmat[,1]), pch=18, cex=5)
points(busStops$lon,busStops$lat, col=alpha(cols3[2], dmat[,2]), pch=18, cex=5)
points(busStops$lon+0.0001, busStops$lat+0.0001,  col=alpha(cols3[3], dmat[,3]), pch=18, cex=5)
points(busStops$lon,busStops$lat, col=rgb(0,0,0), pch=8, cex=1)

#====================================
# Attempt 3 : overlay pie charts with google maps
#====================================
# devtools::install_github("dkahle/ggmap")
# devtools::install_github("hadley/ggplot2")


library(ggplot2) 
library(scatterpie) 
# world <- map_data('Zurich') 
# p <- ggplot(world, aes(long, lat)) + 
#      geom_map(map=world, aes(map_id=region), fill=NA, color="black") + 
#      coord_quickmap() 
# p + geom_scatterpie(aes(x=long, y=lat, group=region, r=radius), data=d, cols=LETTERS[1:4], color=NA, alpha=.8) + 
#     geom_scatterpie_legend(d$radius, x=-160, y=-55) 


ZH <- get_map("Zurich,Switzerland", zoom=16)
p <- ggmap(ZH, alpha=0.5)
d <- data.frame(lat=busStops$lat,lon=busStops$lon)
d1 <- cbind(d, dmat)
p + geom_scatterpie(aes(x=lon, y=lat), data=d1, cols=c("1", "2", "3"), 
                    color=NA, alpha=.8)
  
 
