rm(list=ls())

#====================================
# Simulate or load data
#====================================

#  Simulate, until there is real data

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

#  Read

library(RCurl)

# Landmarks with given classes ('type'). We limited ourselves for three of them
x <- getURL("https://raw.githubusercontent.com/OpenDataDayZurich2016/odd-zurich-stops-classification/master/data-examples/overpass_classes.csv")
locations <- read.csv(text = x)
names(locations) <- c('x','y','type')
head(locations)

# Bus stops 
x <- getURL("https://raw.githubusercontent.com/OpenDataDayZurich2016/odd-zurich-stops-classification/master/data-examples/haltestellen_coordinates.csv")
busStops <- read.csv(text = x)
names(busStops) <- c('x','y')
head(busStops)

busStops <- busStops[sample(1:nrow(busStops),3),]
busStops

# We know there are just 3 classes for now
cols3 <- c('steelblue4','springgreen4','yellow')

cols_locs <- ifelse(locations$type==1,cols3[1],ifelse(locations$type==2,cols3[2],cols3[3]))

#====================================
# Plot raw data
#====================================
# # extent
box  <- data.frame(x=c(47.3726,47.3806), y=c(8.5272,8.5490))
plot(box)
points(busStops, col=rgb(0,0,0), pch=8, cex=1)
points(locations, col=cols_locs, pch=16, cex=1)

#====================================
# Compute travel time by walking
#====================================
require("gmapsdistance")

#dist_mat[i,j] - walking time from bus stop i to landmark j
dist_mat <- matrix(data = NA, nrow = nrow(busStops), ncol = nrow(locations))

# THIS SHOULD NOT BE DONE MANUALLY
for (i in 1:nrow(busStops)){
  for (j in 1:nrow(locations)){
    or <- paste(busStops$x[i],busStops$y[i], sep='+')
    des <- paste(locations$x[j],locations$y[j], sep='+')
    results = gmapsdistance(origin = or, destination = des, mode = "walking")
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
    dist_mat_type[i,type] <- sum(dist_mat_inv[i, which(locations$type==type)])
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
 plot(box)
 points(locations, col=cols_locs, pch=16, cex=1)

# One way to visualize - blend colors, Bad idea - it all looks grey!
 require(scales)
 points(busStops-0.0001, col=alpha(cols3[1], dmat[,1]), pch=18, cex=5)
 points(busStops, col=alpha(cols3[2], dmat[,2]), pch=18, cex=5)
 points(busStops+0.0001, col=alpha(cols3[3], dmat[,3]), pch=18, cex=5)
 points(busStops, col=rgb(0,0,0), pch=8, cex=1)

# Another visualization solution: plot pie charts instead of blending colors.
# Does not work so well. Is it us or is it the package being strange?
# require(caroline)
# col.tab <- nv(cols3, paste('type',unique(locations$type), sep=''))
# # THIS IS VERY BAD IMPLEMENTATION, BUT WE HAVE 4mins LEFT
# pie.list <- list(
#   st1=nv(dmat[1,],c('type1','type2','type3')),
#   st2=nv(dmat[2,],c('type1','type2','type3')),
#   st3=nv(dmat[3,],c('type1','type2','type3')))
# 
# dev.off()
# plot(box)
# pies(x=pie.list, 
#      x0=busStops$x[1:3], 
#      y0=busStops$y[1:3], 
#      radii=5, 
#      show.labels=F, 
#      show.slice.labels=F, 
#      color.table=col.tab)
# Hmmmm..... somethng went wrong here, the weighting should be

