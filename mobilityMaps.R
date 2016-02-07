
# Compute distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  print(a)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# Example:
# earth.dist(41.382071, 2.166479, 41.379849, 2.164099) * 1000 #*1000 to obtain meters

##########################################################################################
################################ EXAMPLE DATASET ########################################
#   "lat1",   "lon1",   "lat2", "lon2",   "createdAt1",       "createdAt2",   "diffTime"
# 1 41.38571 2.176733 41.36877 2.189541 2015-02-15T18:48:33 2015-03-08T13:56:48  29948
# 2 41.36877 2.189541 41.41407 2.152376 2015-03-08T13:56:48 2015-07-18T13:55:47 190018
# 3 41.41407 2.152376 41.41407 2.152376 2015-07-18T13:55:47 2015-07-18T16:14:15    138
# 4 41.38083 2.185807 41.38076 2.185989 2015-01-01T16:30:28 2015-01-01T16:31:13      0
# 5 41.38076 2.185989 41.38347 2.190120 2015-01-01T16:31:13 2015-01-01T17:06:07     34
# 6 41.37372 2.157440 41.39432 2.171173 2015-01-18T12:55:07 2015-03-16T14:03:52  82148
##########################################################################################
##########################################################################################

trayFileName <- "/Users/matteo.manca/Dropbox/Sync/Research-Projects/Projects/Eurecat/urbaning/trayectories/2015-complete/local_flickr_pairs_2015.csv" ## Locals
trays <- read.csv(trayFileName, sep = ' ', header = FALSE)

head(trays)
nms <- c("lat1","lon1","lat2","lon2","createdAt1", "createdAt2", "diffTime")
names(trays) <- nms

##Compute distance in meters between two consecutive points
trays$dist <- earth.dist(trays$lat1,trays$lon1,trays$lat2,trays$lon2) * 1000 

#  check
 head(trays)
# dim(trays)

##I consider two tweets a path only if they have been posted ta leasta at 150m of distance
## and within 10 hours

filteredTrays <- trays[which(trays$dist > 150 & trays$diffTime < 600), ]  #& trays$dist > 200 &
head(filteredTrays)
dim(filteredTrays )

## sort by dist desc to draw first longer lines
filteredTrays <- filteredTrays[with(filteredTrays, order(-dist)), ]

head(filteredTrays)

####################

maxColorValue <- 20
palette <- colorRampPalette(c("#7F0000", "red", "#FF7F00","yellow", "#7FFF7F", "cyan","#007FFF","blue", "#00007F" ))(maxColorValue)
filteredTrays <- filteredTrays[complete.cases(filteredTrays),]

library(ggmap)
require(ggmap)
barcelona <- get_map(location = 'barcelona', zoom = 13, maptype = "terrain", source = "google")
bcnMap <- ggmap(barcelona, extent = "device", colour= "#090D2A", fill="#090D2A") + 
  geom_segment(data=filteredTrays, aes(x=lon1, y=lat1, xend=lon2, yend=lat2), color = palette[cut(log10(filteredTrays$dist), maxColorValue)], alpha=0.4 ) 
bcnMap

ggsave ("localsRoutasFlickr2015-12.png", dpi = 300, height=15,width=20) #this saves the output to a file

