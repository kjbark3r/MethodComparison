#######################################################
## Comparing Methods of Assessing Migratory Behavior ##
##  Maps for TWS presentation - National Mtg 2016    ##
########  NSERP - Kristin Barker - Sept 2016  #########
#######################################################

## WD
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Migration"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Migration"
dir <- if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
      cat("Are you SURE you got that file path right?\n")
  }
}
rm(wd_workcomp, wd_laptop)

## PACKAGES
library(sp) #for kernel centroid estimate
library(adehabitatHR) #for kernel centroid estimate
library(raster)
library(rgdal)
library(gsubfn)
library(maptools) #for writeSpatialShape
library(dplyr) #for between()
library(ggmap)

#DEFINE PROJECTIONS
latlong = CRS("+init=epsg:4326")
  
#### COLLAR DATA ####
elk <- read.csv("collardata-locsonly-equalsampling.csv", header = TRUE)
elk$Date <- as.Date(elk$Date, "%m/%d/%Y")
elk$IndivYr <- ifelse(elk$Date < "2015-01-01", 
                       paste(elk$AnimalID, "-14", sep=""),
                       paste(elk$AnimalID, "-15", sep=""))
elk$MigHR <- ifelse(between(elk$Date, as.Date("2014-01-01"), as.Date("2014-03-15")), "Winter 2014", 
                     ifelse(between(elk$Date, as.Date("2014-07-01"), as.Date("2014-08-31")), "Summer 2014", 
                            ifelse(between(elk$Date, as.Date("2015-01-01"), as.Date("2015-03-15")), "Winter 2015", 
                                   ifelse(between(elk$Date, as.Date("2015-07-01"), as.Date("2015-08-31")), "Summer 2015",
                                          ifelse(between(elk$Date, as.Date("2016-01-01"), as.Date("2016-03-15")), "Winter 2016",
                                                 ifelse(NA))))))
#### BASEMAPS ####

myLoc <- c(-114.515, 45.45399, -113.585, 47.0158) #xmin, ymin, xmax, ymax
myMap <- get_map(location=myLoc, source="google", maptype="terrain", 
                 crop=F, zoom = 9)

myLocN <- c(-114.2, 46.6, -113.8, 46.9)
myMapN <- get_map(location=myLocN, source="google", maptype="terrain")


#### INDIVIDUAL SEASONAL MAPS ####
# note to self: you tried to do these in r
# but it was slow af and crashed, so... arcmap

# 140400-14 - vi and ao call it migrant; looks resident
# because they only take one season into acct

indiv <- subset(elk, AnimalID == 140400)

win <- subset(indiv, MigHR == "Winter 2014")
w140400 <- data.frame("x" = win$Long,"y" = win$Lat)
xy <- data.frame("x"=w140400$x,"y"=w140400$y)
ll <- SpatialPointsDataFrame(xy, w140400, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE140400-w14"), format="GTiff", overwrite=TRUE)

smr <- subset(indiv, MigHR == "Summer 2014")
s140400 <- data.frame("x" = smr$Long,"y" = smr$Lat)
xy <- data.frame("x"=s140400$x,"y"=s140400$y)
ll <- SpatialPointsDataFrame(xy, s140400, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE140400-s14"), format="GTiff", overwrite=TRUE)

win2 <- subset(indiv, MigHR == "Winter 2015")
w2.140400 <- data.frame("x" = win2$Long,"y" = win2$Lat)
xy <- data.frame("x"=w2.140400$x,"y"=w2.140400$y)
ll <- SpatialPointsDataFrame(xy, w2.140400, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE140400-w15"), format="GTiff", overwrite=TRUE)

# 151380-15 - nsd calls it resident; looks migratory
# because mort before return trip could occur

indiv <- subset(elk, AnimalID == 151380)

win <- subset(indiv, MigHR == "Winter 2015")
w151380 <- data.frame("x" = win$Long,"y" = win$Lat)
xy <- data.frame("x"=w151380$x,"y"=w151380$y)
ll <- SpatialPointsDataFrame(xy, w151380, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE151380-w15"), format="GTiff", overwrite=TRUE)

smr <- subset(indiv, MigHR == "Summer 2015")
s151380 <- data.frame("x" = smr$Long,"y" = smr$Lat)
xy <- data.frame("x"=s151380$x,"y"=s151380$y)
ll <- SpatialPointsDataFrame(xy, s151380, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE151380-s15"), format="GTiff", overwrite=TRUE)

# 151300-15 - nsd calls it resident; actually intermediate
# back-and-forth movements

indiv <- subset(elk, AnimalID == 151300)

win <- subset(indiv, MigHR == "Winter 2015")
w151300 <- data.frame("x" = win$Long,"y" = win$Lat)
xy <- data.frame("x"=w151300$x,"y"=w151300$y)
ll <- SpatialPointsDataFrame(xy, w151300, proj4string = latlong)
writeOGR(ll, dir, layer="Pts151300-W15", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE151300-w15"), format="GTiff", overwrite=TRUE)

smr <- subset(indiv, MigHR == "Summer 2015")
s151300 <- data.frame("x" = smr$Long,"y" = smr$Lat)
xy <- data.frame("x"=s151300$x,"y"=s151300$y)
ll <- SpatialPointsDataFrame(xy, s151300, proj4string = latlong)
writeOGR(ll, dir, layer="Pts151300-s15", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE151300-s15"), format="GTiff", overwrite=TRUE)



#### CHECKING OUT DATA TO PICK INDIVS TO MAP USING CODE ABOVE ####
# Possible examples ##

# 151380-15: NSD calls it resident; looks migrant
win <- subset(elk, AnimalID == 151380 & MigHR == "Winter 2015")
w151380 <- data.frame("x" = win$Long,"y" = win$Lat)
smr <- subset(elk, AnimalID == 151380 & MigHR == "Summer 2015")
s151380 <- data.frame("x" = smr$Long,"y" = smr$Lat)
win2 <- subset(elk, AnimalID == 151380 & MigHR == "Winter 2016")
w2.151380 <- data.frame("x" = win2$Long,"y" = win2$Lat)
ggmap(myMap)+
  geom_point(aes(x = x, y = y) , data = w151380, size = 1, color="blue") +
  geom_point(aes(x = x, y = y) , data = s151380, size = 1, color="red") +
  geom_point(aes(x = x, y = y) , data = w2.151380, size = 1, color="black")
# collar stopped functioning before fall migration
# so clearly a migrant, but can't be called that by NSD bc no return trip

# 141490-14: NSD calls it migrant; looks resident
win <- subset(elk, AnimalID == 141490 & MigHR == "Winter 2014")
w141490 <- data.frame("x" = win$Long,"y" = win$Lat)
smr <- subset(elk, AnimalID == 141490 & MigHR == "Summer 2014")
s141490 <- data.frame("x" = smr$Long,"y" = smr$Lat)
win2 <- subset(elk, AnimalID == 141490 & MigHR == "Winter 2015")
w2.141490 <- data.frame("x" = win2$Long,"y" = win2$Lat)
ggmap(myMapN)+
  geom_point(aes(x = x, y = y) , data = w141490, size = 1, color="blue") +
  geom_point(aes(x = x, y = y) , data = s141490, size = 1, color="red") +
  geom_point(aes(x = x, y = y) , data = w2.141490, size = 1, color="black")

# 140400-14 - volume intersect and AO call it migrant; looks resident
win <- subset(elk, AnimalID == 140400 & MigHR == "Winter 2014")
w140400 <- data.frame("x" = win$Long,"y" = win$Lat)
smr <- subset(elk, AnimalID == 140400 & MigHR == "Summer 2014")
s140400 <- data.frame("x" = smr$Long,"y" = smr$Lat)
win2 <- subset(elk, AnimalID == 140400 & MigHR == "Winter 2015")
w2.140400 <- data.frame("x" = win2$Long,"y" = win2$Lat)
ggmap(myMapN)+
  geom_point(aes(x = x, y = y) , data = w140400, size = 1, color="blue") +
  geom_point(aes(x = x, y = y) , data = s140400, size = 1, color="red")+
  geom_point(aes(x = x, y = y) , data = w2.140400, size = 1, color="black")
# this is because volume intersect only considered spring mign (i think)

# 151300-15: multiple back-and-forth
win <- subset(elk, AnimalID == 151300 & MigHR == "Winter 2015")
w151300 <- data.frame("x" = win$Long,"y" = win$Lat)
smr <- subset(elk, AnimalID == 151300 & MigHR == "Summer 2015")
s151300 <- data.frame("x" = smr$Long,"y" = smr$Lat)
#win2 <- subset(elk, AnimalID == 151300 & MigHR == "Winter 2016")
#w2.151300 <- data.frame("x" = win2$Long,"y" = win2$Lat)
ggmap(myMapN) +
  geom_point(aes(x = x, y = y) , data = w151300, size = 1, color="blue") +
  geom_point(aes(x = x, y = y) , data = s151300, size = 1, color="red") #+
  #geom_point(aes(x = x, y = y) , data = w2.151300, size = 1, color="black")
huh <- subset(mig, IndivYr == "151300-15")
plot(huh$SprNSD ~ huh$SprNSDRank, xlim=c(0,60), ylim=c(0,400))  
plot(huh$SprVI ~ huh$SprVIRank, xlim=c(0,60), ylim=c(0,1))
plot(huh$SprAO ~ huh$SprAORank, xlim=c(0,60), ylim=c(0,1))
# nsd classifies as STRONG resident; vi & ao classify as intermed
  
# Less good... ##

# 141340-15: NSD calls it resident; looks migrant
win <- subset(elk, AnimalID == 141340 & MigHR == "Winter 2015")
w141340 <- data.frame("x" = win$Long,"y" = win$Lat)
smr <- subset(elk, AnimalID == 141340 & MigHR == "Summer 2015")
s141340 <- data.frame("x" = smr$Long,"y" = smr$Lat)
ggmap(myMap)+
  geom_point(aes(x = x, y = y) , data = w141340, size = 1, color="blue") +
  geom_point(aes(x = x, y = y) , data = s141340, size = 1, color="red")
# collar stopped functioning before fall migration
# so clearly a migrant, but can't be called that by NSD bc no return trip


# 141130-14: NSD calls it migrant; looks resident
win <- subset(elk, AnimalID == 141130 & MigHR == "Winter 2014")
w141130 <- data.frame("x" = win$Long,"y" = win$Lat)
smr <- subset(elk, AnimalID == 141130 & MigHR == "Summer 2014")
s141130 <- data.frame("x" = smr$Long,"y" = smr$Lat)
#win2 <- subset(elk, AnimalID == 141130 & MigHR == "Winter 2015")
#w2.141130 <- data.frame("x" = win2$Long,"y" = win2$Lat)
ggmap(myMapN)+
  geom_point(aes(x = x, y = y) , data = w141130, size = 1, color="blue") +
  geom_point(aes(x = x, y = y) , data = s141130, size = 1, color="red") +
  geom_point(aes(x = x, y = y) , data = w2.141130, size = 1, color="black")
# hard to pinpoint exact problem - not sure why i called it resident
# some overlap in part of summer range, but not much

# 140100-15: 2 diff summer areas with winter area in middle - but separate concentrated use
huh <- subset(mig, IndivYr == "140040-15")
plot(huh$SprNSD ~ huh$SprNSDRank)  
# NSD classifies it as intermediate, but I think it migrated
plot(huh$SprVI ~ huh$SprVIRank)

# 141170-14: multiple back-and-forth
win <- subset(elk, AnimalID == 141170 & MigHR == "Winter 2014")
w141170 <- data.frame("x" = win$Long,"y" = win$Lat)
smr <- subset(elk, AnimalID == 141170 & MigHR == "Summer 2014")
s141170 <- data.frame("x" = smr$Long,"y" = smr$Lat)
win2 <- subset(elk, AnimalID == 141170 & MigHR == "Winter 2015")
w2.141170 <- data.frame("x" = win2$Long,"y" = win2$Lat)
ggmap(myMapN) +
  geom_point(aes(x = x, y = y) , data = w141170, size = 1, color="blue") +
  geom_point(aes(x = x, y = y) , data = s141170, size = 1, color="red") +
  geom_point(aes(x = x, y = y) , data = w2.141170, size = 1, color="black")
huh <- subset(mig, IndivYr == "141170-14")
plot(huh$SprNSD ~ huh$SprNSDRank, xlim=c(0,60), ylim=c(0,400))  
plot(huh$SprVI ~ huh$SprVIRank, xlim=c(0,60), ylim=c(0,1))
plot(huh$SprAO ~ huh$SprAORank, xlim=c(0,60), ylim=c(0,1))

################# KDEs - PRE-GGMAP ##################

# 140100-15: used two different summer areas with winter area in between ####
# concentrated use different in different seasons, but decent area overlap

# winter2015 - locs and kde
indiv <- subset(elk, AnimalID == 140100 & MigHR == "Winter 2015")
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts140100-W15", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
  writeRaster(raster, paste("KDE140100-W15"), format="GTiff", overwrite=TRUE)
# summer2015 - locs and kde
indiv <- subset(elk, AnimalID == 140100 & MigHR == "Summer 2015") # also check out 2015
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts140100-S15", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE140100-S15"), format="GTiff", overwrite=TRUE)
# nsd line 2015


# 140040-15: intermediate area; non-directed movement. ####
# 1st glance said migrant
#yeah that's pretty obviously a migrant no matter how you slice it

# winter2015 - locs and kde
indiv <- subset(elk, AnimalID == 140040 & MigHR == "Winter 2015")
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts140040-W15", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
  writeRaster(raster, paste("KDE140040-W15"), format="GTiff", overwrite=TRUE)
# summer2015 - locs and kde
indiv <- subset(elk, AnimalID == 140040 & MigHR == "Summer 2015") # also check out 2015
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts140040-S15", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)

# 140320-15: 1st glance: intermediate but id lean towards mig ####

# winter2015 - locs and kde
indiv <- subset(elk, AnimalID == 140320 & MigHR == "Winter 2015")
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts140320-W15", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
  writeRaster(raster, paste("KDE140320-W15"), format="GTiff", overwrite=TRUE)
# summer2015 - locs and kde
indiv <- subset(elk, AnimalID == 140320 & MigHR == "Summer 2015") # also check out 2015
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts140320-S15", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE140320-S15"), format="GTiff", overwrite=TRUE)
mig[mig$IndivYr == "140320-15",c(3, 5, 7, 10, 12, 14)]


## KRISTIN something's screwy with the below code; did you paste wrong?
# 141630-14: 1st glance: back n forth a bit in spring ####

# winter2014 - locs and kde
indiv <- subset(elk, AnimalID == 141630 & MigHR == "Winter 2014")
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts141630-W14", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
  writeRaster(raster, paste("KDE141630-W14"), format="GTiff", overwrite=TRUE)
# summer2014 - locs and kde
indiv <- subset(elk, AnimalID == 141630 & MigHR == "Summer 2014") # also check out 2015
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts141630-S14", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE141630-S14"), format="GTiff", overwrite=TRUE)
mig[mig$IndivYr == "141630-14",c(3, 5, 7, 10, 12, 14)]

# 140120-14: ao says obvs resident, but is obvs migrant ####
# seems like i must have effed up something??
#yeah, winter2014 kde doesn't look right
#ignoring for now; need to focus on TWS poster - 
#BUT COME BACK TO THIS

# winter2015 - locs and kde
indiv <- subset(elk, AnimalID == 140120 & MigHR == "Winter 2014")
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts140120-W14", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
  writeRaster(raster, paste("KDE140120-W14"), format="GTiff", overwrite=TRUE)
# summer2015 - locs and kde
indiv <- subset(elk, AnimalID == 140120 & MigHR == "Summer 2014") 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts140120-S14", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE140120-S14"), format="GTiff", overwrite=TRUE)

# 141490-14: nsd says obvs migrant, but is obvs resident ####

# winter2014 - locs and kde
indiv <- subset(elk, AnimalID == 141490 & MigHR == "Winter 2014")
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts141490-W14", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
  writeRaster(raster, paste("KDE141490-W14"), format="GTiff", overwrite=TRUE)
# summer2014 - locs and kde
indiv <- subset(elk, AnimalID == 141490 & MigHR == "Summer 2014") 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
  writeOGR(ll, dir, layer="Pts141490-S14", driver="ESRI Shapefile")
kde <- kernelUD(ll, h="href", grid = 5000); raster <- raster(kde)
writeRaster(raster, paste("KDE141490-S14"), format="GTiff", overwrite=TRUE)

