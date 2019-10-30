

library("sf")
library("rnaturalearth")
library("geojsonsf")
library("cartography")
library("cartogram")
library("leaflet")
library("SpatialPosition")
library("units")
library("OECD")
library("ggplot2")
library("ggthemes")
library("osmdata")


# -- Source : Natural Earth -- 
# Rivières
rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

# Trait de côte
coastline <- ne_download(scale = 50, type = "coastline", category = "physical", returnclass = "sf")

# Océans 
ocean <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")



# -- Source : Cartographier le monde à l'échelle infranationale (CIST) -- 
subreg <- st_read(dsn = "data/regions/mex_us_admin_1.shp",options = "ENCODING=UTF-8",
                      stringsAsFactors = FALSE)
subreg <- st_transform (subreg, 
                        crs = "+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")


#  DATA -- PIB & demo (level 1-2)
pib <- read.csv("data/regions/PIB.csv", sep = "\t",encoding = "UTF-8", dec = ",",
                stringsAsFactors=FALSE)


pop <- read.csv("data/regions/POP.csv", sep = "\t", encoding = "UTF-8", dec = ",",
                stringsAsFactors=FALSE)


subreg <- merge (x = subreg, y = pib, 
                     by.x = "ID_ADMIN_1",
                     by.y = "ID_ADMIN",
                     all.x = TRUE)

subreg <- merge (x = subreg, y = pop, 
                     by.x = "ID_ADMIN_1",
                     by.y = "ID_ADMIN",
                     all.x = TRUE)




# -- Source : data.world (https://data.world/carlvlewis/border-fence-boundaries-u-s-mexico)
# Mur de séparation
fences <- geojson_sf("data/data.world/border-fence.geojson")





prj <- "+proj=ortho +lat_0=-35 +lon_0=-104 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

subregions <- st_transform(subreg,crs = prj) 
countries <- st_transform(countries,crs = prj) 
fences <- st_transform(fences,crs = prj) 
rivers <- st_transform(rivers,crs = prj) 
coastline <- st_transform(coastline,crs = prj) 
ocean <- st_transform(ocean,crs = prj) 
subregions <- st_transform(subregions,crs = prj) 



# Définition de la bbox
bb <- c(-1668000, 4798000, 934863, 5900000)
d  <- 100000
bbox <- st_as_sfc(st_bbox(c(xmin = bb[1]-2*d , xmax = bb[3]+2*d, ymax = bb[2]-3.5*d, ymin = bb[4]+3.5*d), crs = prj))



# Intersection
subregions <- st_intersection(x = subregions, y = bbox)
rivers <- st_intersection(x = rivers, y = bbox)
coastline <- st_intersection(x = coastline, y = bbox) 


library(cartography)

lay <- function(title = ""){
  authors <- "N. Lambert & R. Ysebaert, 2019\nData source: IOM, Didelon, Vandermotten, Dessouroux, (c) OpenStreetMap contributors, 2019"

par(mar = c(0,0,1.2,0))

#plot(st_geometry(bbox), col = "#b8d5e3", border = NA)
#plot(st_geometry(bboxtop), col =NA, border = NA)
plot(st_geometry(bbox), col= "#b8d5e3", border = NA, xlim = bb[c(1,3)], ylim = bb[c(2,4)])
plot(st_geometry(subregions) + c(-10000, -10000), col ="#827e6c50", border = NA, add = T)
plot(st_geometry(subregions), col= "#ede6bb", border = "white", cex = 0.5, add=T)
plot(st_geometry(coastline), col= "#6d9cb3",lwd = 1 ,add= T)
plot(st_geometry(rivers), col= "#6d9cb3",lwd = 1 ,add= T)
plot(st_geometry(fences), col= "#3d3c3c",lwd = 3 ,add= T)
layoutLayer(title = title,
            author =  authors,
            scale = 300, south = TRUE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white")

}
lay("Template cartographique")






par(mar = c(0,0,1.2,0))

lay("Une barrière démographique...")

choroLayer(x = subregions, var = "POP65_POP15",
           breaks = c(min(subregions$POP65_POP15, na.rm = T),
                      20,25,35,50,65, max(subregions$POP65_POP15, na.rm = T)),
           col = carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "red.pal", n2 = 3),
           legend.pos = "left",
           legend.horiz = TRUE, legend.title.cex = 0.7, legend.values.cex = 0.5,
           legend.title.txt = "Rapport entre la population âgée de plus de 65 ans\net la population âgée de moins de 15 ans\nen 2015 (%)",
           border = NA, add = TRUE)

plot(st_geometry(coastline), col= "#6d9cb3",lwd = 1 ,add= T)

# Get borders
subregions.borders <- getBorders(subregions)

discLayer(x = subregions.borders, df = subregions,
          var = "POP65_POP15", col="black", nclass=3,
          method="equal", threshold = 0.3, sizemin = 0.5,
          sizemax = 10, type = "abs",legend.values.rnd = 0,
          legend.title.txt = "Discontinuités sur l'indice de veillissement 2015\n(différences absolues)",
          legend.pos = c(-1710000,4640000), legend.title.cex = 0.7, legend.values.cex = 0.5,
          add = TRUE)




lay("Doublé d'un mur de richesse... Mais quelles conséquences ?")

choroLayer(x = subregions, var = "PIB100_2017",
           breaks = c(min(subregions$PIB100_2017, na.rm = T),
                      75,90,100,125,150,200, max(subregions$PIB100_2017, na.rm = T)),
           col = carto.pal(pal1 = "red.pal", n1 = 3, pal2 = "green.pal", n2 = 4),
           legend.pos = "left",
           legend.horiz = TRUE, legend.title.cex = 0.7, legend.values.cex = 0.5,
           legend.title.txt = "PIB par habitant 2017\n(100 = moyenne mondiale)",
           border = NA,
           add = TRUE)

plot(st_geometry(coastline), col= "#6d9cb3",lwd = 1 ,add= T)

# Get borders
discLayer(x = subregions.borders, df = subregions,
          var = "PIB100_2017", col="black", nclass=3,
          method="equal", threshold = 0.2, sizemin = 0.5,
          sizemax = 10, type = "abs",legend.values.rnd = 0,
          legend.title.txt = "Discontinuités de PIB par habitant 2017\n(différences absolues)",
          legend.pos = c(-1710000,4640000), legend.title.cex = 0.7, legend.values.cex = 0.5,
          add = TRUE)




library(cartogram)

lay("Une autre visualisation de la pop ?")

# Gestion des multipolygones




subreg <- st_cast(subregions, "MULTIPOLYGON")

# Calcul cartogramme
subregAnam <- cartogram_cont(subreg,weight = "POP_2015", itermax = 30)


layoutLayer(title = "Et si on déformait sur la population ?",
            scale = FALSE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white", bg = "#b8d5e3")

plot(st_geometry(subreg), border = NA, col = "#827e6c50", add = TRUE)

# Cartographie
choroLayer(x = subregAnam, var = "PIB100_2017",
           breaks = c(min(subregAnam$PIB100_2017, na.rm = T),
                      75,90,100,125,150,200, max(subregAnam$PIB100_2017, na.rm = T)),
           col = carto.pal(pal1 = "red.pal", n1 = 3, pal2 = "green.pal", n2 = 5),
           legend.pos = "topleft",
           legend.title.cex = 0.7, legend.values.cex = 0.5,
           legend.title.txt = "PIB par habitant 2017\n(100 = moyenne mondiale)",
           border = "white", add = T)


# Et discontinuités associées - DO NOT WORK
# subregAnamBor <- getBorders(subregAnam)
# 
# discLayer(x = subregAnamBor, df = subregions,
#           var = "PIB100_2017", col="black", nclass=3,
#           method="equal", threshold = 0.2, sizemin = 0.5,
#           sizemax = 10, type = "abs",legend.values.rnd = 0,
#           legend.title.txt = "Discontinuités de PIB \npar habitant 2017\n(différences absolues)",
#           legend.pos = "left", legend.title.cex = 0.7, legend.values.cex = 0.5,
#           add = TRUE)




# DO NOT WORK

# # Convertir la bounding box en WGS 84
# tmp <- st_transform(bbox, 4326)
# 
# # Définir la requête (clé/valeur OSM sur bounding box)
# opqbox <- opq(bbox = tmp , timeout = 5000)
# opquery <- add_osm_feature(opq = opqbox, key = "barrier", value = "border_control")
# feat <- osmdata_sf(opquery)
# 
# # Extraire les points qui répondent à la requête
# featpt <- st_transform(feat$osm_points, prj)
# featpt <- featpt[featpt[["barrier"]] %in% "border_control", ]
# 
# # Extraire les polygones qui répondent à la requête
# featpo <- st_transform(feat$osm_polygons, prj)
# st_geometry(featpo) <- st_centroid(st_geometry(featpo))
# featpo$osm_id <- row.names(featpo)
# 
# # Combiner points et polygones, les intersecter avec la bounding box
# featpt <- rbind(featpt[, c("osm_id", "geometry")], featpo[, c("osm_id", "geometry")])
# poi_osm <- st_intersection(x = featpt, st_geometry(subregions))
# 
# 
# lay("Localisation des postes frontaliers")
# plot(st_geometry(featpt), bg = "red", col = NA, pch = 21, cex = 0.8, add = T)
# 
# plot(st_geometry(bbox))
# featpt





# Import du fichier brut (OIM)
iom <- read.csv("data/iom/MissingMigrants-Global-2019-10-29T14-11-50.csv", stringsAsFactors = F)

# Gestion des coordonnées
iom <- iom[(iom$Location.Coordinates)!="",]
latlon <- matrix(as.numeric(unlist(strsplit(iom$Location.Coordinates, split = ", "))), ncol = 2, byrow = T)
colnames(latlon) <- c("lat", 'lon')
iom <- cbind(iom, latlon)

# Sélection et renommage des variables, conversion au format numérique des champs
iom <- iom[,c("Web.ID","Reported.Year","Total.Dead.and.Missing","Number.of.Survivors","Region.of.Incident","lat","lon")]
colnames(iom) <- c("id","year","deads","survivors","region","latitude","longitude")
iom$deads <- as.numeric(iom$deads)
iom <- iom[!is.na(iom$deads),]
iom$latitude <- as.numeric(iom$latitude)
iom$longitude <- as.numeric(iom$longitude)

# Conversion en objet sf et reprojection
iom_sf <- st_as_sf(iom, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
iom_sf <- st_transform(iom_sf,crs = prj)

lay("USA-Mexico border")
plot(st_geometry(iom_sf), pch=20, col= "#eb3850", cex = 0.5, add=T)


lay("USA-Mexico border")
propSymbolsLayer(x = iom_sf, var = "deads",
                 symbols = "circle", col =  "#eb3850",
                 legend.pos = "left", border = "black", lwd = 0.5,
                 legend.title.txt = "Dead\nand missing\nmigrants,\n2014 - 2019",
                 legend.style = "e")



# # Doesn't work
# iom_sf$m_weight <- 1
# iom_sf$m_weight[iom_sf$deads > 1] <- 0.5
# iom_sf$m_weight[iom_sf$deads >= 25] <- 0
# deathsdor <- cartogram_dorling(x = st_jitter(iom_sf),weight = "deads", m_weight = iom_sf$m_weight, k = .4)
# 
# lay("USA-Mexico border")
# plot(st_geometry(deathsdor), pch=20, col= "#eb3850", border ="#ede6bb", cex = 0.1, add=T)
# plot(st_geometry(fences), col= "#3d3c3c", lwd = 3 ,add= T)
# 
# 
# # 
# all <- iom_sf[,c("id","deads","year","geometry")]
# 
# iom_unique <- all[all$deads == 1,]
# iom_multi <-  all[all$deads > 1,]
# 
# for (i in 1:dim(iom_multi)[1]){
#   nb <- as.numeric(iom_multi[i,"deads"])[1]
#   tmp <- iom_multi[i,]
#   tmp$deads <- 1
#   for (j in 1:nb){ iom_unique <- rbind(iom_unique,tmp)}
# }
# 
# deathsdor2 <- cartogram_dorling(x = st_jitter(iom_unique),weight = "deads", k = .004)
# lay("USA-Mexico border")
# plot(st_geometry(deathsdor2), pch=20, col= "#eb3850", border ="#ede6bb", cex = 0.1, add=T)
# plot(st_geometry(fences), col= "#3d3c3c", lwd = 3 ,add= T)


