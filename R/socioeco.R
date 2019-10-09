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


# --- admin level 12
subregions <- st_read(dsn = "../data/regions/mex_us_can_admin_12.shp",options = "ENCODING=UTF-8",
                      stringsAsFactors = FALSE)

subregions <- st_transform(subregions,crs = prj) 



# -- PIB & demo (level 1-2)
pib <- read.csv("../data/regions/PIB.csv", sep = "\t", encoding = "UTF-8", dec = ",",
                stringsAsFactors=FALSE)


subregions <- merge (x = subregions, y = pib, 
                     by.x = "ID_ADMIN",
                     by.y = "ID_ADMIN",
                     all.x = TRUE)





# IDH dans le temps / PIB > Ro 



#---- 1 Analyse des discontinuites > Ro 
# Démo + PIB en vis-à-vis 
# Projection orthodromique

lay("USA-Mexico border")

breaks <- c(min(subregions$PIB100_2013, na.rm = T),75,90,100,110,125,150,200,300, max(subregions$PIB100_2013, na.rm = T))

choroLayer(x = subregions, var = "PIB100_2013",
           breaks = breaks,
           col = carto.pal(pal1 = "blue.pal", n1 = 3,
                           pal2 = "red.pal", n2 = 6),
           border = "grey40")


# Lissage gros pas (population) / cercles proportionnels
# PIB par habitant / anamorphose sur la pop 



# 2 - OSM > Ronan
# Amenity = police
# Carroyage différentes portées - Hexagones
# Fences

library(osmdata)

bbox <- st_transform(iom_sf, crs = 4326)
q0 <- opq(bbox = c(st_bbox(bbox)[1], st_bbox(bbox)[2], st_bbox(bbox)[3], st_bbox(bbox)[4])) 

q1 <- add_osm_feature(opq = q0, key = 'man_made', value = "surveillance")


iom_sf

bbox <- st_as_sfc(ylim = st_bbox(iom_sf)[c(2,4)], xlim = st_bbox(iom_sf)[c(1,3)],
                  crs = st_crs(4326))


plot(st_geometry(ocean), col= "#b8d5e3", border = NA, ylim = st_bbox(iom_sf)[c(2,4)], xlim = st_bbox(iom_sf)[c(1,3)])

plot(st_geometry(countries), add = TRUE)

# --- MAP 5 : Grid ----


