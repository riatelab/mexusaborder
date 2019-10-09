# --------------------------------- #
#               DEAD                #
#           AND MISSING             #
#             MIGRANTS              #
# --------------------------------- #

library("sf")
library("rnaturalearth")
library("geojsonsf")
library("cartography")
library("cartogram")
library("SpatialPosition")

# ********************************
# ********** GEOMETRIES **********
# ********************************

prj <- "+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# --- COUNTRIES ---

countries <- ne_countries(scale = 50, type = "countries", continent = NULL,
                          country = NULL, geounit = NULL, sovereignty = NULL,
                          returnclass = "sf")
countries <- countries[countries$adm0_a3 %in% c("MEX","USA"),]
countries <- st_transform(countries,crs = prj) 

# --- FENCES ---

# https://data.world/carlvlewis/border-fence-boundaries-u-s-mexico
# https://www.revealnews.org/article/the-wall-building-a-continuous-u-s-mexico-barrier-would-be-a-tall-order/

fences <- geojson_sf("../data/data.world/border-fence.geojson")
fences <- st_transform(fences,crs = prj) 

# --- RIVERS ---

rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
rivers <- st_transform(rivers,crs = prj) 

# --- COASTS ---

coastline <- ne_download(scale = 50, type = "coastline", category = "physical", returnclass = "sf")
coastline <- st_transform(coastline,crs = prj) 

# --- ocean ---

ocean <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")
ocean <- st_transform(ocean,crs = prj) 


# ********************************
# ************* DATA *************
# ********************************

# --- Missing migrants

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







# ********************************
# ************* CHARTS *************
# ********************************

# Regional Comparison 


par(mar=c(10,8,4,2))
med <- aggregate(iom_sf$deads,list(iom_sf$region), sum, simplify = TRUE )
colnames(med) <- c("region","nb")
total <- round(sum(med$nb),-2)
cols <- rep("#ffbaba",length(med$region))
cols[c(7,15)] <- "red"
barplot(med$nb, ylab="Number of persons", names.arg=med$region, las=2, border="#991313",col=cols)

# Time Trend

iom_sf <- iom_sf[iom_sf$region =="US-Mexico Border",]

par(mar=c(5,8,4,2))
med <- aggregate(iom_sf$deads,list(iom_sf$year), sum, simplify = TRUE )
colnames(med) <- c("year","nb")
total <- round(sum(med$nb),-2)
med[med$year==2019,"year"] <- "2019*"
barplot(med$nb, xlab=paste0("Total over the perdiod: ",total,"\n(*) from January 1 to Sept. 3 2019"), ylab="Number of persons", names.arg=med$year,
        border="#991313",col=c("red","red","red","red","red","#ffbaba"))


# ********************************
# ********** CARTOGRAPHY *********
# ********************************

# --- MAP TEMPLATE ---

lay <- function(title = ""){
  authors <- "N. Lambert & R. Ysebaert, 2019\nData source: IOM, 2019"
  par(mar = c(0,0,1.2,0))
  plot(st_geometry(ocean), col= "#b8d5e3", border = NA, ylim = st_bbox(iom_sf)[c(2,4)], xlim = st_bbox(iom_sf)[c(1,3)])
  plot(st_geometry(countries) + c(-10000, -10000), col ="#827e6c50", border = NA, add= T)
  plot(st_geometry(countries), col= "#ede6bb", border = "white", cex = 0.5, add=T)
  plot(st_geometry(coastline), col= "#6d9cb3",lwd = 1 ,add= T)
  plot(st_geometry(rivers), col= "#6d9cb3",lwd = 1 ,add= T)
  plot(st_geometry(fences), col= "#3d3c3c",lwd = 3 ,add= T)
  layoutLayer(title = title,
              author =  authors,
              scale = 300, south = TRUE, frame = TRUE,
              col = "#6d9cb3", coltitle = "white")
}


# typoLayer(x = fences, var="gen_type",
#           col = c("red", "blue"),
#           legend.values.order = c("pedestrian","vehicle"),
#           legend.pos = "topleft",
#           legend.title.txt = "USA - MEX border",
#           legend.nodata = "Unknown", add= T, lwd = 10)

# --- MAP 1 ---

lay("USA-Mexico border")
plot(st_geometry(iom_sf), pch=20, col= "#eb3850", cex = 0.5, add=T)

# --- MAP 2 : Prop symbols ----

lay("USA-Mexico border")
propSymbolsLayer(x = iom_sf, var = "deads",
                 symbols = "circle", col =  "#eb3850",
                 legend.pos = "left", border = "black", lwd = 0.5,
                 legend.title.txt = "Dead\nand missing\nmigrants,\n2014 - 2019",
                 legend.style = "e")

# --- MAP 3 : Dorling ----
iom_sf$m_weight <- 1
iom_sf$m_weight[iom_sf$deads > 1] <- 0.5
iom_sf$m_weight[iom_sf$deads >= 25] <- 0
deathsdor <- cartogram_dorling(x = st_jitter(iom_sf),weight = "deads", m_weight = iom_sf$m_weight, k = .4)

lay("USA-Mexico border")
plot(st_geometry(deathsdor), pch=20, col= "#eb3850", border ="#ede6bb", cex = 0.1, add=T)
plot(st_geometry(fences), col= "#3d3c3c", lwd = 3 ,add= T)

# --- MAP 4 : Dorling (disintegrated) ----

all <- iom_sf[,c("id","deads","year","geometry")]

iom_unique <- all[all$deads == 1,]
iom_multi <-  all[all$deads > 1,]

for (i in 1:dim(iom_multi)[1]){
nb <- as.numeric(iom_multi[i,"deads"])[1]
tmp <- iom_multi[i,]
tmp$deads <- 1
for (j in 1:nb){ iom_unique <- rbind(iom_unique,tmp)}
}

deathsdor2 <- cartogram_dorling(x = st_jitter(iom_unique),weight = "deads", k = .004)
lay("USA-Mexico border")
plot(st_geometry(deathsdor2), pch=20, col= "#eb3850", border ="#ede6bb", cex = 0.1, add=T)
plot(st_geometry(fences), col= "#3d3c3c", lwd = 3 ,add= T)

# --- MAP 5 : LEAFLET ----

library(leaflet)

# Data import and shaping
iom <- read.csv("../data/iom/MissingMigrants-Global-2019-09-04T11-59-55.csv", stringsAsFactors = F)
iom <- iom[(iom$Location.Coordinates)!="",]
iom <- iom[iom$Region.of.Incident =="US-Mexico Border",]
latlon <- matrix(as.numeric(unlist(strsplit(iom$Location.Coordinates, split = ", "))), ncol = 2, byrow = T)
colnames(latlon) <- c("lat", 'lon')
iom <- cbind(iom, latlon)
iom <- iom[,c("Reported.Year","Total.Dead.and.Missing","lat","lon","Location.Description","Cause.of.Death","Information.Source")]
colnames(iom) <- c("year","deads","lat","lon","location","cause","source")

fences <- geojson_sf("../data/data.world/border-fence.geojson")

# Disaggregation

iom_unique <- iom[iom$deads == 1,]
iom_multi <-  iom[iom$deads > 1,]
for (i in 1:dim(iom_multi)[1]){
  nb <- as.numeric(iom_multi[i,"deads"])[1]
  tmp <- iom_multi[i,]
  tmp$deads <- 1
  for (j in 1:nb){ iom_unique <- rbind(iom_unique,tmp)}
}

iom <- iom_unique

pins <- makeIcon(
  iconUrl = "../data/pin.svg",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 15
)

iom$label <- paste0(
                    "<h1>",iom$cause,"</h1>
                     <h3>year: </b>",iom$year,"<br/>
                     location: ",iom$location,"</h3>                      
                     <i>Source: ",iom$source,"</i>"
                    )

m <- leaflet(iom) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  setView(lng = -104, lat = 30, zoom = 06) %>%
  addMarkers(~lon, ~lat, popup = ~label, clusterOptions = markerClusterOptions(), icon = pins ) %>%
  addScaleBar(position = "bottomleft") %>%
addPolylines(data = fences, color = "black", weight = 7, opacity = 1)
m



