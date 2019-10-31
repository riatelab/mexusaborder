
authors <- "N. Lambert & R. Ysebaert, 2019\nData source: IOM, Didelon, Vandermotten, Dessouroux, (c) OpenStreetMap contributors, 2019"


################
# 0 - Appel des librairies
################

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



################
# 1 - Import des géométries
################

# Pays
countries <- ne_countries(scale = 50, type = "countries", continent = NULL,
                          country = NULL, geounit = NULL, sovereignty = NULL,
                          returnclass = "sf")
countries <- countries[countries$adm0_a3 %in% c("MEX","USA"),]

# Rivières
rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

# Trait de côte
coastline <- ne_download(scale = 50, type = "coastline", category = "physical", returnclass = "sf")

# Océans 
ocean <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")

# -- Source : Cartographier le monde à l'échelle infranationale (CIST) -- 
subregions <- st_read(dsn = "data/regions/mex_us_admin_1.shp",options = "ENCODING=UTF-8",
                      stringsAsFactors = FALSE)

# [TO DO - Régler pb topologiques]


# -- Source : data.world (https://data.world/carlvlewis/border-fence-boundaries-u-s-mexico)
# Mur de séparation
fences <- geojson_sf("data/data.world/border-fence.geojson")



################
# 2 - Mise en forme des géométries et création d'un template [Projection Albers]
################

# choix de la projection

albers <- "+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
countries_aea <- st_transform(countries,crs = albers) 
fences_aea <- st_transform(fences,crs = albers) 
rivers_aea <- st_transform(rivers,crs = albers) 
coastline_aea <- st_transform(coastline,crs = albers) 
ocean_aea <- st_transform(ocean,crs = albers) 
subregions_aea <- st_transform(subregions,crs = albers) 

# Choix de l'emprise (un peu de bidouille, pas optimal)

bb_aea <- c(-1342784.0, -739750.5, 793341.2, 1317849.8)
d <- 100000
bbox_aea <- st_as_sfc(st_bbox(c(xmin = bb_aea[1]-2*d , xmax = bb_aea[3]+2*d, ymax = bb_aea[2]-d, ymin = bb_aea[4]+d), crs = albers))


# Intersection du fond régional avec la bbox

subregions_aea <- st_intersection(x = subregions_aea, st_geometry(bbox_aea))
coastline_aea <- st_intersection(x = coastline_aea, y = bbox_aea) 
rivers_aea <- st_intersection(x = rivers_aea, y = bbox_aea)
countries_aea <- st_intersection(x = countries_aea, y = bbox_aea) 

# création du template

lay_aea <- function(title = ""){
par(mar = c(0,0,1.2,0))
plot(st_geometry(ocean_aea), col= "#b8d5e3", border = NA, xlim = bb_aea[c(1,3)], ylim = bb_aea[c(2,4)])
plot(st_geometry(subregions_aea) + c(-10000, -10000), col ="#827e6c50", border = NA, add = T)
plot(st_geometry(subregions_aea), col= "#ede6bb", border = "white", cex = 0.5, add=T)
plot(st_geometry(coastline_aea), col= "#6d9cb3",lwd = 1 ,add= T)
plot(st_geometry(rivers_aea), col= "#6d9cb3",lwd = 1 ,add= T)
plot(st_geometry(fences_aea), col= "#3d3c3c",lwd = 3 ,add= T)
layoutLayer(title = title,
            author =  authors,
            scale = 300, south = TRUE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white")
}

sizes_aea <- getFigDim(x = bbox_aea, width = 1500,mar = c(0,0,1.2,0), res = 150)
png("outputs/fig01.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)
lay_aea("Template cartographique 1 (projection Albers)")
dev.off()



################
# 3 - Mise en forme des géométries et création d'un template [Projection Orthographique]
################

# choix de la projection

ortho <- "+proj=ortho +lat_0=-35 +lon_0=-104 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
countries_ortho <- st_transform(countries,crs = ortho) 
fences_ortho <- st_transform(fences,crs = ortho) 
rivers_ortho <- st_transform(rivers,crs = ortho) 
coastline_ortho <- st_transform(coastline,crs = ortho) 
ocean_ortho <- st_transform(ocean,crs = ortho) 
subregions_ortho <- st_transform(subregions,crs = ortho) 

# Choix de l'emprise (un peu de bidouille, pas optimal)

bb_ortho <- c(-1668000, 5100000, 934863, 5900000)

d  <- 100000
bbox_ortho <- st_as_sfc(st_bbox(c(xmin = bb_ortho[1]-2*d , xmax = bb_ortho[3]+2*d, ymax = bb_ortho[2]-3.5*d, ymin = bb_ortho[4]+3.5*d), crs = ortho))

# Intersection du fond régional avec la bbox

subregions_ortho <- st_intersection(x = subregions_ortho, st_geometry(bbox_ortho))
rivers_ortho <- st_intersection(x = rivers_ortho, y = bbox_ortho)
coastline_ortho <- st_intersection(x = coastline_ortho, y = bbox_ortho) 

# création du template

lay_ortho <- function(title = ""){
  par(mar = c(0,0,1.2,0))
  plot(st_geometry(bbox_ortho), col= "#b8d5e3", border = NA, xlim = bb_ortho[c(1,3)], ylim = bb_ortho[c(2,4)])
  plot(st_geometry(subregions_ortho) + c(-10000, -10000), col ="#827e6c50", border = NA, add = T)
  plot(st_geometry(subregions_ortho), col= "#ede6bb", border = "white", cex = 0.5, add=T)
  plot(st_geometry(coastline_ortho), col= "#6d9cb3",lwd = 1 ,add= T)
  plot(st_geometry(rivers_ortho), col= "#6d9cb3",lwd = 1 ,add= T)
  plot(st_geometry(fences_ortho), col= "#3d3c3c",lwd = 3 ,add= T)
  layoutLayer(title = title,
              author =  authors,
              scale = 300, south = TRUE, frame = TRUE,
              col = "#6d9cb3", coltitle = "white")
  
}

sizes_ortho <- getFigDim(x = bbox_ortho, width = 1500,mar = c(0,0,1.2,0), res = 150)
png("outputs/fig02.png", width = sizes_ortho[1], height = sizes_ortho[2], res = 150)
lay_ortho("Template cartographique 2 (projection orthographique)")
dev.off()

# Template bis avec les murs en 2.5D

lay_ortho2 <- function(title = ""){
  authors <- "N. Lambert & R. Ysebaert, 2019\nData source: IOM, Didelon, Vandermotten, Dessouroux, (c) OpenStreetMap contributors, 2019"
  par(mar = c(0,0,1.2,0))
  plot(st_geometry(bbox_ortho), col= "#b8d5e3", border = NA, xlim = bb_ortho[c(1,3)], ylim = bb_ortho[c(2,4)])
  plot(st_geometry(subregions_ortho) + c(-10000, -10000), col ="#827e6c50", border = NA, add = T)
  plot(st_geometry(subregions_ortho), col= "#ede6bb", border = "white", cex = 0.5, add=T)
  plot(st_geometry(coastline_ortho), col= "#6d9cb3",lwd = 1 ,add= T)
  plot(st_geometry(rivers_ortho), col= "#6d9cb3",lwd = 1 ,add= T)
  plot(st_geometry(fences_ortho), col= "#3d3c3c",lwd = 2 ,add= T)
  line <- st_geometry(fences_ortho)
  for (i in 1:20){
    line <- st_geometry(line) + c(0,5000)
    plot(st_geometry(line), col= "#565b6380",lwd = 2 ,add= T)  
  }
  plot(st_geometry(line), col= "#3d3c3c",lwd = 2 ,add= T) 
  layoutLayer(title = title,
              author =  authors,
              scale = 300, south = TRUE, frame = TRUE,
              col = "#6d9cb3", coltitle = "white")
  
}

sizes_ortho <- getFigDim(x = bbox_ortho, width = 1500,mar = c(0,0,1.2,0), res = 150)
png("outputs/fig03.png", width = sizes_ortho[1], height = sizes_ortho[2], res = 150)
lay_ortho2("Template cartographique 2 (projection orthographique)")
dev.off()

################
# 4 - Import des données et jointures
################

#  DATA -- PIB & demo (level 1-2)

pib <- read.csv("data/regions/PIB.csv", sep = "\t",encoding = "UTF-8", dec = ",",
                stringsAsFactors=FALSE)


pop <- read.csv("data/regions/POP.csv", sep = "\t", encoding = "UTF-8", dec = ",",
                stringsAsFactors=FALSE)


subregions_aea <- merge (x = subregions_aea, y = pib, 
                     by.x = "ID_ADMIN_1",
                     by.y = "ID_ADMIN",
                     all.x = TRUE)

subregions_aea <- merge (x = subregions_aea, y = pop, 
                     by.x = "ID_ADMIN_1",
                     by.y = "ID_ADMIN",
                     all.x = TRUE)

subregions_ortho <- merge (x = subregions_ortho, y = pib, 
                         by.x = "ID_ADMIN_1",
                         by.y = "ID_ADMIN",
                         all.x = TRUE)

subregions_ortho <- merge (x = subregions_ortho, y = pop, 
                         by.x = "ID_ADMIN_1",
                         by.y = "ID_ADMIN",
                         all.x = TRUE)

################
# 5 - Evolution du PIB par habitant dans le temps
################

# Télécharger les données de la table PDB_LV pour USA, Mexique, pays de l'OCDE

df <- get_dataset(dataset = "PDB_LV", filter = list(c("MEX", "USA","OECD"), 
                                                    "T_GDPPOP",
                                                    "CPC"))

# Transformer la date au format numérique

df$obsTime <- as.numeric(df$obsTime)

# Représentation graphique

png("outputs/fig04.png", width = 1500, height = 1000, res = 150)
ggplot(data = df, aes(x = obsTime, y = obsValue, color = LOCATION)) + 
  geom_line(size = 1) +  
  labs(x = NULL, y = "Dollars, prix courant", color = NULL,
       title =  "Évolution comparée du PIB par habitant (Mexique - USA - OCDE)") +
  theme_hc() +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
dev.off()


################
# 6 - Evolution des moins de 20 ans dans le temps
################

df <- as.data.frame(get_dataset(dataset = "POP_PROJ", 
                                filter = list(c("MEX","USA","OECD"),"TT","D1TTR5Y4")))

df$obsTime <- as.numeric(df$obsTime)


# Représentation graphique

png("outputs/fig05.png", width = 1500, height = 1000, res = 150)
ggplot(data = df, aes(x = obsTime, y = obsValue, color = LOCATION)) + 
  geom_line(size = 1) +  
  labs(x = NULL, y = "Part de la population totale", color = NULL,
       title =  "Évolution de la part des moins de 20 ans (Mexique - USA - OCDE)") +
  theme_hc() +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
dev.off()


############### 
# 7 - Cartographie des ruptures spatiales : Vieillissement démographique
###############

# Version 1 ------

png("outputs/fig06.png", width = sizes_aea[1], height = sizes_aea[2], res = 150)


par(mar = c(0,0,1.2,0))
plot(st_geometry(bbox_aea), col= "#b8d5e3", border = NA, xlim = bb_aea[c(1,3)], ylim = bb_aea[c(2,4)])

choroLayer(x = subregions_aea, var = "POP65_POP15",
           breaks = c(min(subregions_aea$POP65_POP15, na.rm = T),
                      20,25,35,50,65, max(subregions_aea$POP65_POP15, na.rm = T)),
           col = carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "red.pal", n2 = 3),
           legend.pos = c(-1400000 , -600000),
           legend.horiz = TRUE, legend.title.cex = 0.7, legend.values.cex = 0.5,
           legend.title.txt = "Rapport entre la population âgée de plus de 65 ans\net la population âgée de moins de 15 ans\nen 2015 (%)",
           border = NA, add = TRUE)

subregions.borders <- getBorders(subregions_aea)

discLayer(x = subregions.borders, df = subregions_aea,
          var = "POP65_POP15", col="black", nclass=3,
          method="equal", threshold = 0.3, sizemin = 0.5,
          sizemax = 10, type = "abs",legend.values.rnd = 0,
          legend.title.txt = "Discontinuités sur l'indice de veillissement 2015\n(différences absolues)",
          legend.pos = c(-1400000 , -300000), legend.title.cex = 0.7, legend.values.cex = 0.5,
          add = TRUE)


plot(st_geometry(coastline_aea), col= "#6d9cb3",lwd = 1 ,add= T)


layoutLayer(title = "Une barrière démographique...",
            author =  authors,
            scale = 300, south = TRUE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white")

dev.off()

# Version 2 ------

png("outputs/fig07.png", width = sizes_ortho[1], height = sizes_ortho[2], res = 150)

par(mar = c(0,0,1.2,0))
plot(st_geometry(bbox_ortho), col= "#b8d5e3", border = NA, xlim = bb_ortho[c(1,3)], ylim = bb_ortho[c(2,4)])

choroLayer(x = subregions_ortho, var = "POP65_POP15",
           breaks = c(min(subregions_ortho$POP65_POP15, na.rm = T),
                      20,25,35,50,65, max(subregions_ortho$POP65_POP15, na.rm = T)),
           col = carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "red.pal", n2 = 3),
           legend.pos = c(-1700000, 5000000),
           legend.horiz = TRUE, legend.title.cex = 0.7, legend.values.cex = 0.5,
           legend.title.txt = "Rapport entre la population âgée de plus de 65 ans\net la population âgée de moins de 15 ans\nen 2015 (%)",
           border = NA, add = TRUE)

subregions.borders <- getBorders(subregions_ortho)

discontinuities <- discLayer(x = subregions.borders, df = subregions_ortho,
          var = "POP65_POP15", col="black", nclass=3,
          method="equal", threshold = 0.3, sizemin = 0.5,
          sizemax = 10, type = "abs",legend.values.rnd = 0,
          legend.title.txt = "Discontinuités sur l'indice\nde veillissement 2015\n(différences\nabsolues)",
          legend.pos = c(-1700000, 5300000), legend.title.cex = 0.7, legend.values.cex = 0.5,
          add = TRUE)

plot(st_geometry(coastline_ortho), col= "#6d9cb3",lwd = 1 ,add= T)


layoutLayer(title = "Une barrière démographique...",
            author =  authors,
            scale = 300, south = TRUE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white")

dev.off()

# Version 3 ------

threshold <- 0.3
minvar <- as.numeric(quantile(discontinuities$disc, probs = c(1 - threshold)))
discontinuities <- discontinuities[discontinuities$disc >= minvar,]
discontinuities$height <- round(discontinuities$disc / 2,0)

extrude <- function(id){
  line <- st_geometry(discontinuities[id,])
  plot(line, col= "black",lwd = 2 ,add= T)
  nb <- as.numeric(discontinuities[id,"height"])[1]
  for (j in 1:nb){
    line <- st_geometry(line) + c(0,5000)
    plot(st_geometry(line), col= "#ebd23480",lwd = 2 ,add= T)  
  }
  plot(line, col= "black",lwd = 2 ,add= T)
}



png("outputs/fig08.png", width = sizes_ortho[1], height = sizes_ortho[2], res = 150)

par(mar = c(0,0,1.2,0))
plot(st_geometry(bbox_ortho), col= "#b8d5e3", border = NA, xlim = bb_ortho[c(1,3)], ylim = bb_ortho[c(2,4)])

choroLayer(x = subregions_ortho, var = "POP65_POP15",
           breaks = c(min(subregions_ortho$POP65_POP15, na.rm = T),
                      20,25,35,50,65, max(subregions_ortho$POP65_POP15, na.rm = T)),
           col = carto.pal(pal1 = "green.pal", n1 = 3, pal2 = "red.pal", n2 = 3),
           legend.pos = c(-1700000, 5000000),
           legend.horiz = TRUE, legend.title.cex = 0.7, legend.values.cex = 0.5,
           legend.title.txt = "Rapport entre la population âgée de plus de 65 ans\net la population âgée de moins de 15 ans\nen 2015 (%)",
           border = NA, add = TRUE)


plot(st_geometry(coastline_ortho), col= "#6d9cb3",lwd = 1 ,add= T)
for (i in 1:length(discontinuities$disc))
{
  extrude(i)
}
legtxt <- "Sur cette carte, la hauteur\ndes barrières est proportionnelle\nà la valeur des discontinuités\nabsolues sur l'indice du\nvieillissement en 2015."
text(-1700000, y = 5400000, legtxt  , cex = 0.9, pos = 4, font = 2) 
layoutLayer(title = "Une barrière démographique...",
            author =  authors,
            scale = 300, south = TRUE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white")

dev.off()

############### 
# 8 -  Cartographie des ruptures spatiales : PIB par habitant
###############

lay("Doublé d'un mur de richesse... Mais quelles conséquences ?")

choroLayer(x = subregions, var = "PIB100_2017",
           breaks = c(min(subregions$PIB100_2017, na.rm = T),
                      75,90,100,125,150,200, max(subregions$PIB100_2017, na.rm = T)),
           col = carto.pal(pal1 = "red.pal", n1 = 3, pal2 = "green.pal", n2 = 4),
           legend.pos = "topleft",
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
          legend.pos = "left", legend.title.cex = 0.7, legend.values.cex = 0.5,
          add = TRUE)



############
# 9 - Anamorphose population / richesse
#############

# Gestion des multipolygones
subregions <- st_cast(subregions, "MULTIPOLYGON")

# Calcul cartogramme
subregions_anam <- cartogram_cont(subregions,weight = "POP_2015", itermax = 30)

# Cartographie
choroLayer(x = subregions_anam, var = "PIB100_2017",
           breaks = c(min(subregions_anam$PIB100_2017, na.rm = T),
                      75,90,100,125,150,200, max(subregions_anam$PIB100_2017, na.rm = T)),
           col = carto.pal(pal1 = "red.pal", n1 = 3, pal2 = "green.pal", n2 = 5),
           legend.pos = "topleft",
           legend.horiz = TRUE, legend.title.cex = 0.7, legend.values.cex = 0.5,
           legend.title.txt = "PIB par habitant 2017\n(100 = moyenne mondiale)",
           border = "white")

# Et discontinuités associées
subregions_anamBor <- getBorders(subregions_anam)

discLayer(x = subregions_anamBor, df = subregions,
          var = "PIB100_2017", col="black", nclass=3,
          method="equal", threshold = 0.2, sizemin = 0.5,
          sizemax = 10, type = "abs",legend.values.rnd = 0,
          legend.title.txt = "Discontinuités de PIB par habitant 2017\n(différences absolues)",
          legend.pos = "left", legend.title.cex = 0.7, legend.values.cex = 0.5,
          add = TRUE)


layoutLayer(title = "Et si on déformait sur la population ?",
            scale = FALSE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white")



############
# 10 - Border Control - Visualisation des postes de contrôle
#############

# Convertir la bounding box en WGS 84
bbox <- st_transform(bbox, 4326)

# Définir la requête (clé/valeur OSM sur bounding box)
opqbox <- opq(bbox = bbox , timeout = 5000)
opquery <- add_osm_feature(opq = opqbox, key = "barrier", value = "border_control")
feat <- osmdata_sf(opquery)

# Extraire les points qui répondent à la requête
featpt <- st_transform(feat$osm_points, prj)
featpt <- featpt[featpt[["barrier"]] %in% "border_control", ]

# Extraire les polygones qui répondent à la requête
featpo <- st_transform(feat$osm_polygons, prj)
st_geometry(featpo) <- st_centroid(st_geometry(featpo))
featpo$osm_id <- row.names(featpo)

# Combiner points et polygones, les intersecter avec la bounding box
featpt <- rbind(featpt[, c("osm_id", "geometry")], featpo[, c("osm_id", "geometry")])
poi_osm <- st_intersection(x = featpt, st_geometry(subregions))

# Représentation des points extraits
lay("Localisation des postes frontaliers")
plot(st_geometry(poi_osm), bg = "red", col = NA, pch = 21, cex = 0.8, add = TRUE)



############
# 11 - Border Control - Densité #1
#############

# Créer une grille sur l'espace d'étude
grid <- st_make_grid(subregions, cellsize = 50000)
grid <- st_sf(grid)

# Compter le nombre de postes de police par points de grille
grid$ncops <- lengths(st_covers(grid, poi_osm))
#grid$dcops <- grid$ncops / set_units(st_area(grid), "km^2")

# Plusieurs façons de visualiser les résultats (tests)
lay("Densité de postes frontaliers")
propSymbolsLayer(grid, var = "ncops", col = "red", symbols = "square", add = T,
                 legend.pos = "topright",
                 legend.title.cex = 0.7, legend.values.cex = 0.6,
                 legend.title.txt = "Nombre de postes frontière\n(zones de 50km²)")



############
# 12 - Border Control - Densité #2
#############

lay("Des tours de contrôle ?")
choroLayer(x = grid, var = "ncops",
           breaks = c(0.535, 5, 10, 20, 40, 66),
           col = carto.pal(pal1 = "brown.pal", n1 = 5),
           legend.pos = "topright",
           legend.title.cex = 0.7, legend.values.cex = 0.6,
           legend.title.txt = "Nombre de postes frontière\n(zones de 50km²)",
           border = NA,
           add = TRUE)


############
# 13 - Border Control - Densité #3
#############

# [TO DO - REPRESENTATION 3D de ces éléments de grille ?]





############
# 14 - Morts aux frontières - Import et mise en forme des données
#############


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





############
# 15 - Morts aux frontières - Comparaisons régionales
#############

par(mar=c(8,8,4,2))

# Agréger pae zone géographique
med <- aggregate(iom_sf$deads,list(iom_sf$region), sum, simplify = TRUE )

# Sélection des colonnes utiles, arrondis
colnames(med) <- c("region","nb")
total <- round(sum(med$nb),-2)

# Ordonner nombre de morts par zone géographique
med <- med[order(med$nb,decreasing = TRUE),]

# Gestion des couleurs
cols <- rep("#ffbaba",length(med$region))
cols[c(3)] <- "red"

# Barplot
barplot(med$nb, ylab = "Nombre de personnes", names.arg = med$region, las = 2, 
        border="#991313",col = cols, cex.names = 0.8, cex.axis = 0.8)





############
# 16 - Morts aux frontières - Evolution temporelle
#############

# Extraction de la zone USA-Mexique
iom_sf <- iom_sf[iom_sf$region =="US-Mexico Border",]

#st_bbox(iom_sf) > no comprendo l'interesta
par(mar=c(5,8,4,2))

# Agréger par années 
med <- aggregate(iom_sf$deads,list(iom_sf$year), sum, simplify = TRUE )

# Gestion des labels, arrondis
colnames(med) <- c("year","nb")
total <- round(sum(med$nb),-1)

# Un petit label spécial pour 2019 (année en cours)
med[med$year==2019,"year"] <- "2019*"

# Barplot
barplot(med$nb, xlab=paste0("Total sur la période: ",total,"\n(*) Du 1er janvier au 29 octobre 2019"), 
        ylab="Nombre de personnes", names.arg=med$year,
        border="#991313",col=c("red","red","red","red","red","#ffbaba"),
        cex.names = 0.8, cex.axis = 0.8)



############
# 17 - Morts aux frontières - Carte de localisation
#############

lay("USA-Mexico border")
plot(st_geometry(iom_sf), pch=20, col= "#eb3850", cex = 0.5, add=T)




############
# 18 - Morts aux frontières - Localisation et nombre de personnes
#############

lay("USA-Mexico border")
propSymbolsLayer(x = iom_sf, var = "deads",
                 symbols = "circle", col =  "#eb3850",
                 legend.pos = "left", border = "black", lwd = 0.5,
                 legend.title.txt = "Dead\nand missing\nmigrants,\n2014 - 2019",
                 legend.style = "e")

# Idée : carte animée, combiner graphique + figurés proportionnels par année



############
# 19 - Morts aux frontières - Cartogramme de Dorling 
#############

iom_sf$m_weight <- 1
iom_sf$m_weight[iom_sf$deads > 1] <- 0.5
iom_sf$m_weight[iom_sf$deads >= 25] <- 0
deathsdor <- cartogram_dorling(x = st_jitter(iom_sf),weight = "deads", m_weight = iom_sf$m_weight, k = .4)

lay("USA-Mexico border")
plot(st_geometry(deathsdor), pch=20, col= "#eb3850", border ="#ede6bb", cex = 0.1, add=T)
plot(st_geometry(fences), col= "#3d3c3c", lwd = 3 ,add= T)




############
# 20 - Morts aux frontières - Cartogramme de Dorling (avec désagrégation)
#############

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





############
# 21 - Carte interactive des morts aux frontières avec Leaflet
#############


iom <- read.csv("data/iom/MissingMigrants-Global-2019-09-04T11-59-55.csv", stringsAsFactors = F)
iom <- iom[(iom$Location.Coordinates)!="",]
iom <- iom[iom$Region.of.Incident =="US-Mexico Border",]
latlon <- matrix(as.numeric(unlist(strsplit(iom$Location.Coordinates, split = ", "))), ncol = 2, byrow = T)
colnames(latlon) <- c("lat", 'lon')
iom <- cbind(iom, latlon)
iom <- iom[,c("Reported.Year","Total.Dead.and.Missing","lat","lon","Location.Description","Cause.of.Death","Information.Source")]
colnames(iom) <- c("year","deads","lat","lon","location","cause","source")

fences <- geojson_sf("data/data.world/border-fence.geojson")

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
  iconUrl = "data/pin.svg",
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





