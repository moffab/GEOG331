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
  NDVIraster[[i]] <- raster(paste0("/Users/benjaminmoffa/Documents/Github/GEOG331/data2/NDVI/NDVI_",ndviYear[i],".tif"))
}

str(NDVIraster[[1]])
NDVIraster[[1]]@crs


#Question #3
#try overlaying raster data and 1966 polygons (wont work)
plot(NDVIraster[[1]])
plot(g1966, col="black", add=TRUE)

#map 2003 ndvi raster data besside 1966 glacial extent
par(mfrow=c(1,2))
plot(NDVIraster[[1]], axes=FALSE)
plotRGB(rgbL, stretch="lin", axes=FALSE)
plot(g1966, col="red", border=NA, add=TRUE, axes=FALSE)
#End Question 3

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#Question 4: plot ndvi with 2015 glaciers
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(g2015p, border="black", add=TRUE, fill=NA)

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#join glacier daata
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

#plot area of each glacier
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
}


#Question 5
#calculate area change for each glacier between 1966 and 2005
for(i in 1:39){
  g2015p@data$areachange[i] =gAll$a2015m.sq[i]-gAll$a1966m.sq[i]
}
#plot area change
spplot(g2015p, "areachange")
#End question 5


# Calculate plot difference in glaciers
diffPoly <- gDifference(g1966p, g2015p, checkValidity = 2L)
plot(diffPoly)

#plot glacal difference with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)


#Question 6
#make subset with glacier that experienced greatest loss
x1<-subset(gAll, gAll$areachange==min(gAll$areachange))

#NEED TO ADD TITLE 
#show glacial recession for glacier with most recession 
plotRGB(rgbL, ext=c(265000,271000,5420000,5430000), stretch="lin", margins=TRUE, axes=FALSE, main=c("Agassiz Glacier", "Area Change:", min(gAll$areachange), "m.sq"))
plot(subset(g1966, g1966$GLACNAME=="Agassiz Glacier"), col="palegreen2", border=NA, add=TRUE, Main="Percent Loss")
plot(subset(g1998, g1966$GLACNAME=="Agassiz Glacier"), col="royalblue3", add=TRUE, border=NA)
plot(subset(g2005, g1966$GLACNAME=="Agassiz Glacier"), col="darkgoldenrod4", add=TRUE, border=NA)
plot(subset(g2015, g1966$GLACNAME=="Agassiz Glacier"), col="tomato3", add=TRUE, border=NA)
#End Question 6

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}
maxValue(NDVIraster[[4]])
plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)


#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)

#create 500m buffer zone around glacial polygon
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#rasterize glacial polygons with 500m buffer
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
#plot
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

#calculate mean change in glacZones
meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)
mean(meanChange[,2])

#Question 9
#add zonal means to 2015 glacier polygons
g2015p@data$zonemean<-0
for(i in 1:39){
  g2015p@data$zonemean[i] =meanChange[i+1,2]
}
#plot zonal means and glaciars 
spplot(g2015p, "zonemean")


#Question 11
#loop through all NDVI years
NDVImean<-list()
for(i in 1:length(ndviYear)){
NDVImean[i]<-cellStats(NDVIraster[[i]], stat='mean', na.rm=TRUE)
}
#get average max NDVI of all years
mean(unlist(NDVImean))

#get range
NDVIfit2<-calc(NDVIstack, min)
plot(NDVIfit2)
NDVIfit3<-calc(NDVIstack, max)
plot(NDVIfit3)
NDVI4<-NDVIfit3-NDVIfit2
plot(NDVI4)

#extract NDVI range from polygons
averagerange <- raster::extract(NDVI4,      
                            g2015p, 
                            buffer =0,  
                            df=TRUE)

#assign average range to g2015p data for each glacier
g2015p@data$averagerange1<-0
for(i in 1:39){
  g2015p@data$averagerange1[i] =averagerange[i,2]
}

#color code glacial range
g2015p@data$color[g2015p@data$averagerange1<0.25]<-"red"
g2015p@data$color[g2015p@data$averagerange1<0.14]<-"blue"
g2015p@data$color[g2015p@data$averagerange1<0.08]<-"black"

#specify that NDVIraster is stack (repeat from earlier)
NDVIstack <- stack(NDVIraster)
#get plot of average NDVI 
NDVIfit1<-calc(NDVIstack, mean)

#plot average NDVI and glaciers color coded by range of their NDVI 
plot(NDVIfit1, axes=FALSE) 
plot(g2015p, col=g2015p@data$color, border=NA, add=TRUE)









