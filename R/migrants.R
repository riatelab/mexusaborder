# Dead and missig migrants (IOM data)

library("sf")
library("rnaturalearth")
library("geojsonsf")
library("cartography")

# ********************************
# ********** GEOMETRIES **********
# ********************************

prj <- "+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# --- COUNTRIES ---

countries <- ne_countries(scale = 50, type = "countries", continent = NULL,
                          country = NULL, geounit = NULL, sovereignty = NULL,
                          returnclass = "sf")

countries <- st_transform(countries,crs = prj) 

# --- FENCES ---

# https://data.world/carlvlewis/border-fence-boundaries-u-s-mexico
# https://www.revealnews.org/article/the-wall-building-a-continuous-u-s-mexico-barrier-would-be-a-tall-order/

fences <- geojson_sf("../data/data.world/border-fence.geojson")
plot(st_geometry(fences))
fences <- st_transform(fences,crs = prj) 

# --- RIVERS ---

rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
rivers <- st_transform(rivers,crs = prj) 

# --- COASTS ---

coastline <- ne_download(scale = 50, type = "coastline", category = "physical", returnclass = "sf")
coastline <- st_transform(coastline,crs = prj) 


# ********************************
# ************* DATA *************
# ********************************

iom <- read.csv("../data/iom/MissingMigrants-Global-2019-09-04T11-59-55.csv", stringsAsFactors = F)
iom <- iom[(iom$Location.Coordinates)!="",]

latlon <- matrix(as.numeric(unlist(strsplit(iom$Location.Coordinates, split = ", "))), ncol = 2, byrow = T)
colnames(latlon) <- c("lat", 'lon')
iom <- cbind(iom, latlon)
iom <- iom[,c("Web.ID","Reported.Year","Total.Dead.and.Missing","Number.of.Survivors","Region.of.Incident","lat","lon")]
colnames(iom) <- c("id","year","deads","survivors","region","latitude","longitude")
iom$deads <- as.numeric(iom$deads)
iom <- iom[!is.na(iom$deads),]
iom$latitude <- as.numeric(iom$latitude)
iom$longitude <- as.numeric(iom$longitude)
iom_sf <- st_as_sf(iom, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
iom_sf <- st_transform(iom_sf,crs = prj) 
iom_sf <- iom_sf[iom_sf$region =="US-Mexico Border",]

# ********************************
# ********** CARTOGRAPHY *********
# ********************************

plot(st_geometry(iom_sf), pch=20, col= "grey", cex = 0.5)
plot(st_geometry(countries), col= "#CCCCCC", border = NA, cex = 0.5, add= T)
plot(st_geometry(iom_sf), pch=20, col= "black", cex = 0.5, add=T)
plot(st_geometry(rivers), col= "blue",lwd = 2 ,add= T)
plot(st_geometry(fences), col= "black",lwd = 5 ,add= T)

