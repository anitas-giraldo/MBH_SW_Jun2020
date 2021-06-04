swbruvs
plot(b2)
proj4string(b2)
points(swbruvs)
depth <- raster::extract(b2, swbruvs, df=T)
min(depth$SW_reefs)
latlongraster <- raster("~/MBHdesignGB/SpatialData/GB_CMR_bathy.tif")
proj4string(latlongraster)

## transform coordinates

utm1 <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
latlong2 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


l1 <- readOGR("~/MBHdesignSW/design4/LobsterClusters_2020-06-01.shp")
l2 <- readOGR("~/MBHdesignSW/design4/LobsterClusters2_2020-06-01.shp")  
l3 <- readOGR("~/MBHdesignSW/design4/LobsterClusters3_2020-06-01.shp")

l1.2 <- spTransform(l1, latlong2)
l2.2 <- spTransform(l2, latlong2)
l3.2 <- spTransform(l3, latlong2)

b3 <- projectRaster(b2, crs=latlong2)
plot(b3, main="Lobster pot clusters 1")
points(l1.2, pch = 20, col='black')
writeOGR(l1.2, dsn="~/MBHdesignSW/design4", layer=paste( "LobsterLatlong1", Sys.Date(), sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)

plot(b3, main="Lobster pot clusters 2")
points(l2.2, pch = 20, col='black')
writeOGR(l2.2, dsn="~/MBHdesignSW/design4", layer=paste( "LobsterLatlong2", Sys.Date(), sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)

plot(b3, main="Lobster pot clusters 3")
points(l3.2, pch = 20, col='black')
writeOGR(l3.2, dsn="~/MBHdesignSW/design4", layer=paste( "LobsterLatlong3", Sys.Date(), sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)

