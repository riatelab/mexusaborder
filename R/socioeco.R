# --------------------------------- #
#         DISCONTINUITIES           #
#         ALONG THE BORDER          #
# --------------------------------- #

library("sf")
library("rnaturalearth")
library("geojsonsf")
library("cartography")
library("cartogram")
library("SpatialPosition")
library(units)

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




#################################################
# ANALYSE D'EVOUTIONS PIB ET DEMO DANS LE TEMPS
#################################################

library(OECD)
library(ggplot2)

as.data.frame(search_dataset("GDP", data = get_datasets()))

dstruc <- get_data_structure("PDB_LV")
str(dstruc, max.level = 2)

dstruc$VAR
dstruc$INDICATOR
dstruc$MEASURE
dstruc$POWERCODE

df <- get_dataset(dataset = "PDB_LV", filter = list(c("MEX", "USA","OECD"), 
                                                    "T_GDPPOP",
                                                    "CPC"))

df$obsTime <- as.numeric(df$obsTime)

qplot(data = df, x = obsTime, y = obsValue, color = LOCATION, geom = "line") +
  labs(x = NULL, y = "USD, current prices, current PPPs", color = NULL,
       title = "GDP per head of population")


# Moins de 20 ans
as.data.frame(search_dataset("population", data = get_datasets()))
dstruc <- get_data_structure("POP_PROJ")

dstruc$VAR_DESC
dstruc$AGE

filter_list <- list(c("MEX","USA","OECD"),"TT","D1TTR5Y4")
df <- as.data.frame(get_dataset(dataset = "POP_PROJ", filter = filter_list))

df$obsTime <- as.numeric(df$obsTime)

qplot(data = df, x = obsTime, y = obsValue, color = LOCATION, geom = "line") +
  labs(x = NULL, y = "%", color = NULL,
       title = "Part de la population âgée de moins de 20 ans")




#################################################
# DES RUPTURES DANS L'ESPACE
#################################################

# --- admin level 12
subregions <- st_read(dsn = "../data/regions/mex_us_admin_1.shp",options = "ENCODING=UTF-8",
                      stringsAsFactors = FALSE)

subregions <- st_transform(subregions,crs = prj) 


#  DATA -- PIB & demo (level 1-2)
pib <- read.csv("../data/regions/PIB.csv", sep = "\t", encoding = "UTF-8", dec = ",",
                stringsAsFactors=FALSE)

pop <- read.csv("../data/regions/POP.csv", sep = "\t", encoding = "UTF-8", dec = ",",
                stringsAsFactors=FALSE)


subregions <- merge (x = subregions, y = pib, 
                     by.x = "ID_ADMIN_1",
                     by.y = "ID_ADMIN",
                     all.x = TRUE)

subregions <- merge (x = subregions, y = pop, 
                     by.x = "ID_ADMIN_1",
                     by.y = "ID_ADMIN",
                     all.x = TRUE)


# Intersect with bounding box (pas optimal)
bbox <- st_as_sfc(st_bbox(c(xmin = -1832288 , xmax = 1500000, ymax = 1450000, ymin = -830000), 
                          crs = prj))
bboxocean <- st_as_sfc(st_bbox(c(xmin = -1342784, xmax = 93341.2, ymin = -739750.5, ymax = 1317850)))

subregions1 <- st_intersection(x = subregions, st_geometry(bbox))



########################
# DISCONTINUITIES - AGE
########################

par(mar = c(0,0,1.2,0))

# Indice de vieillessement
plot(st_geometry(ocean), col= "#b8d5e3", border = NA, ylim = st_bbox(iom_sf)[c(2,4)], xlim = st_bbox(iom_sf)[c(1,3)])
plot(st_geometry(countries) + c(-10000, -10000), col ="#827e6c50", border = NA, add= T)
plot(st_geometry(countries), col= "#ede6bb", border = "white", cex = 0.5, add=T)

choroLayer(x = subregions1, var = "POP65_POP15",
           breaks = c(min(subregions1$POP65_POP15, na.rm = T),
                      25,35,50,65,80,
                      max(subregions1$POP65_POP15, na.rm = T)),
           col = carto.pal(pal1 = "green.pal", n1 = 3,
                           pal2 = "red.pal", n2 = 3),
           legend.pos = "topleft",
           legend.title.txt = "Rapport entre la population âgée de plus de 65 ans\net la population âgée de moins de 15 ans en 2015 (%)",
           border = NA,
           add = TRUE)


# Get borders
subregions.borders <- getBorders(subregions1)

discLayer(x = subregions.borders, df = subregions1,
          var = "POP65_POP15", col="black", nclass=3,
          method="equal", threshold = 0.4, sizemin = 0.5,
          sizemax = 10, type = "abs",legend.values.rnd = 0,
          legend.title.txt = "Discontinuités sur l'indice de veillissement 2015\n(différences absolues)",
          legend.pos = "left", add=TRUE)

plot(st_geometry(coastline), col= "#6d9cb3",lwd = 1 ,add= T)
layoutLayer(title = "Une barrière démographique... ",
            author =  "N. Lambert & R. Ysebaert, 2019\nData source: Didelon, Vandermotten, 2019",
            sources = "Didelon, Vandermotten, Dessouroux, 2019",
            scale = 300, south = TRUE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white")


##########################
# DISCONTINUITIES - GDP
##########################

plot(st_geometry(ocean), col= "#b8d5e3", border = NA, ylim = st_bbox(bboxocean)[c(2,4)],
     xlim = st_bbox(bboxocean)[c(1,3)])
plot(st_geometry(countries) + c(-10000, -10000), col ="#827e6c50", border = NA, add= T)
plot(st_geometry(countries), col= "#ede6bb", border = "white", cex = 0.5, add=T)

choroLayer(x = subregions1, var = "PIB100_2017",
           breaks = c(min(subregions1$PIB100_2017, na.rm = T),
                      75,90,100,125,150,200,
                      max(subregions1$PIB100_2017, na.rm = T)),
           col = carto.pal(pal1 = "red.pal", n1 = 3,
                           pal2 = "green.pal", n2 = 5),
           legend.pos = "topleft",
           legend.title.txt = "PIB par habitant 2017\n(100 = moyenne mondiale)",
           border = NA,
           add = TRUE)

# Get borders
discLayer(x = subregions.borders, df = subregions1,
          var = "PIB100_2017", col="black", nclass=3,
          method="equal", threshold = 0.2, sizemin = 0.5,
          sizemax = 10, type = "abs",legend.values.rnd = 0,
          legend.title.txt = "Discontinuités de PIB par habitant 2017\n(différences absolues)",
          legend.pos = "left", add=TRUE)

plot(st_geometry(coastline), col= "#6d9cb3",lwd = 1 ,add= T)

layoutLayer(title = "Doublé d'un mur de richesse... Mais quelles conséquences ?",
            author =  "N. Lambert & R. Ysebaert, 2019\nData source: Didelon, Vandermotten, OpenStreetMap et contributeurs, 2019",
            sources = "Didelon, Vandermotten, Dessouroux, 2019",
            scale = 300, south = TRUE, frame = TRUE,
            col = "#6d9cb3", coltitle = "white")

# > To do éventuellement > voir ce que ça donne en projection orthodromique ? Pas trouvé les paramètres de mon côté...


#################
# BORDER CONTROL
# ###############

library(osmdata)

# Convert bbox in WGS84
bbox <- st_transform(bbox, 4326)

# define and launch the query
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
featpt <- rbind(featpt[, c("osm_id", "geometry")], featpo[, c("osm_id", "geometry")])
poi_osm <- st_intersection(x = featpt, st_geometry(subregions1))

# Représentation des points extraits
plot(st_geometry(fences), col = "red", add = TRUE)
plot(st_geometry(rivers), col = "lightblue", add = TRUE)
plot(st_geometry(poi_osm), bg = "orange", col = "black", pch = 21, cex = 0.8, add = TRUE)

# Create grid 
grid <- st_make_grid(subregions1, cellsize = 50000)
grid <- st_sf(grid)

# Compter le nombre de postes de police par points de grille
grid$ncops <- lengths(st_covers(grid, poi_osm))
grid$dcops <- grid$ncops / set_units(st_area(grid), "km^2")

# Plusieurs façons de visualiser les résultats (tests)
grid_ncont <- cartogram_ncont(grid, "ncops")
plot(grid_ncont$geometry, col="red", add = T)

propSymbolsLayer(grid, var = "ncops", col = "red", symbols = "square", add = T,
                 legend.pos = "topright",
                 legend.title.txt = "Nombre de postes frontière\n(zones de 50km²)")

choroLayer(x = grid, var = "ncops",
           breaks = c(0.535, 5, 10,20,66),
           col = carto.pal(pal1 = "pink.pal", n1 = 4),
           legend.pos = "topright",
           legend.title.txt = "Nombre de postes frontière\n(zones de 50km²)",
           border = NA,
           add = TRUE)

    
  
# Carto 3d de la densité des postes frontière, à faire  
# library(mapdeck)
# set_token(Sys.getenv("MAPBOX"))
# crash_data = read.csv("https://git.io/geocompr-mapdeck")
# crash_data = na.omit(crash_data)
# ms = mapdeck_style("dark")
# mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) %>%
#   add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
#            elevation_scale = 50, layer_id = "grid_layer",
#            colour_range = viridisLite::plasma(6))
#   
# ms = mapdeck_style("dark")
# mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) %>%
#   add_grid(data = grid, lat = "lat", lon = "lng", cell_size = 1000,
#            elevation_scale = 50, layer_id = "grid",
#            colour_range = viridisLite::plasma(6))




# PIB par habitant / anamorphose sur la pop 

library(cartogram)
subregions <- st_cast(subregions, "MULTIPOLYGON")

subregions_cont <- cartogram_dorling(subregions,weight = "POP_2015")
subregions_cont <- cartogram_cont(subregions,weight = "POP_2015", itermax = 50)

choroLayer(x = subregions_cont, var = "PIB100_2017",
           breaks = c(min(subregions_cont$PIB100_2017, na.rm = T),
                      75,90,100,125,150,200,
                      max(subregions_cont$PIB100_2017, na.rm = T)),
           col = carto.pal(pal1 = "red.pal", n1 = 3,
                           pal2 = "green.pal", n2 = 5),
           legend.pos = "topleft",
           legend.title.txt = "PIB par habitant 2017\n(100 = moyenne mondiale)",
           border = "white")

