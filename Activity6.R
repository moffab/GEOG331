install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("/Users/benjaminmoffa/Documents/Github/GEOG331/data2/GNPglaciers/GNPglaciers_1966.shp", stringsAsFactors = T)

g1998 <- readOGR("/Users/benjaminmoffa/Documents/Github/GEOG331/data2/GNPglaciers/GNPglaciers_1998.shp", stringsAsFactors = T)

g2005 <- readOGR("/Users/benjaminmoffa/Documents/Github/GEOG331/data2/GNPglaciers/GNPglaciers_2005.shp", stringsAsFactors = T)

g2015 <- readOGR("/Users/benjaminmoffa/Documents/Github/GEOG331/data2/GNPglaciers/GNPglaciers_2015.shp", stringsAsFactors = T)

str(g2015)
head(g2015@data)
g2015@polygons[[1]]
g1966@proj4string
#use spatial reference.org or epsg.io
#https://www.georepository.com/crs_26912/NAD83-UTM-zone-12N.html

#make map of glaciers by name
spplot(g1966, "GLACNAME")

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#read in rgb imagery from landsat
redL <- raster("/Users/benjaminmoffa/Documents/Github/GEOG331/data2/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/benjaminmoffa/Documents/Github/GEOG331/data2/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/benjaminmoffa/Documents/Github/GEOG331/data2/glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#zoomed in plot
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("data\\NDVI\\NDVI_",ndviYear[i],".tif"))
  
}

