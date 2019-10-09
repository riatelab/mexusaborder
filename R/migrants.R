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
plot(st_geometry(fences))
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

iom <- read.csv("data/iom/MissingMigrants-Global-2019-09-04T11-59-55.csv", stringsAsFactors = F)
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






# 3 - Graphiques (mises en contexte)
# Comparatif dans le temps 
# ********************************
# ************* CHARTS *************
# ********************************

par(mar=c(5,8,4,2))
med <- aggregate(iom_sf$deads,list(iom_sf$year), sum, simplify = TRUE )
colnames(med) <- c("year","nb")
total <- round(sum(med$nb),-2)
med[med$year==2019,"year"] <- "2019*"
barplot(med$nb, xlab=paste0("Total over the perdiod: ",total,"\n(*) from January 1 to Sept. 3 2019"), ylab="Number of persons", names.arg=med$year,
        border="#991313",col=c("red","red","red","red","red","#ffbaba"))

# Mexique - USA / Mediterranee 





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

# --- MAP 4 : Smoothing ----

# res <- 15000
# span <- 20000
# unk <- CreateGrid(w = as(countries, 'Spatial'), res = res, returnclass = "sf")
# plot(st_geometry(unk))
# 
# stew <- stewart(knownpts = as(iom_sf, 'Spatial'), varname = "deads", 
#                 typefct = "exponential", beta = 2, unknownpts = unk, span = span, 
#                 returnclass = "sf")
# summary(stew$OUTPUT)
# contour <- isopoly(x = stew, breaks = c(1, 2,5,10,20,50, 75, 100, 150, 170), returnclass = "sf")
# 
# lay("USA-Mexico border")
# plot(st_geometry(contour), add=T, col="#CCCCCC")





# --- MAP 5 : Animated map ----

# --- MAP 6 : 3D ? ----

# --- MAP 7 : Interactive ? ----


