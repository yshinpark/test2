rm(list = ls())

# reference: https://rpubs.com/technocrat/thematic_map_state_names
# referebce: https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/

# https://www.census.gov/library/reference/code-lists/ansi.html
# Area Name	FIPS State Numeric Code	Official USPS Code	Status
# American Samoa	60	AS	1
# Federated States of Micronesia	64	FM	3
# Guam	66	GU	1
# Marshall Islands	68	MH	3
# Commonwealth of the Northern Mariana Islands	69	MP	1
# Palau	70	PW	3
# Puerto Rico	72	PR	1
# U.S. Minor Outlying Islands	74	UM	2
# U.S. Virgin Islands	78	VI	1

library(tidyverse)
library(rgdal)
library(maptools)
library(mapproj)
library(rgeos)
library(ggplot2)

us_map<-readOGR(dsn = "C:/Users/rvp8/OneDrive - CDC/Infoaid/tl_2020_us_state/tl_2020_us_state.shp")
plot(us_map)


#convert it to Albers equal area

# Albers Equal Area projection
us_map_aea <- spTransform(us_map, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 
                                 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")) 
us_map_aea@data$id<-rownames(us_map_aea@data)

#alaska
alaska<-us_map_aea[us_map_aea$STATEFP=="02",]
alaska<-elide(alaska, rotate = -25)
alaska<-elide(alaska, scale = max(apply(bbox(alaska), 1, diff))/2.3)
alaska<-elide(alaska, shift=c(-3100000, -100000))
#proj4string(alaska)<-proj4string(us_map_aea)
slot(alaska, "proj4string") <- slot(us_map_aea, "proj4string")
#plot(alaska)

#hawaii

# hawaii <- us_map_aea[us_map_aea$STATEFP=="15",]
# plot(hawaii)
# 
# # # Set x and y limits for the plot
# # ylims <- c(19.0, 23.0) #lat
# # xlims <- c(-160.0, -153.0) #long
# library(sp)
# x_coords <-c(-160.0, -160.0, -153.0, -153.0)
# y_coords <-c(19.0, 19.0, 23.0, 23.0)
# 
# poly<-sp::Polygon(cbind(x_coords, y_coords))
# library(RSAGA)
# res <- rsaga.intersect.polygons(layer_a = poly,
#                                 layer_b = hawaii,
#                                 result = hawaii_pol,
#                                 load = TRUE)
# # plot input polygons
# # Create sf for this bounding box
# bounding.box <- st_as_sf(data.frame(long = xlims, lat = ylims), coords = c("lat", "long"), crs = CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
# # Try to find features that intersect
# intersecting.hawaii <- st_intersection(hawaii, bounding.box)

# extract, then rotate & shift hawaii
 hawaii <- us_map_aea[us_map_aea$STATEFP=="15",]
 hawaii <- elide(hawaii, rotate=-35)
 #hawaii = elide(hawaii, scale=max(apply(bbox(hawaii), 1, diff)) / 5)
 hawaii <- elide(hawaii, shift=c(5400000, -1800000))
#proj4string(hawaii) <- proj4string(us_map_aea)
 slot(hawaii, "proj4string") <- slot(us_map_aea, "proj4string")
 plot(hawaii)

us_main<-us_map_aea[!us_map_aea$STATEFP %in% c("02","15","60","66","69","72","78"),]
plot(us_main)

#puerto rico
puerto_rico = us_map_aea[us_map_aea$STATEFP=="72",]
puerto_rico = elide(puerto_rico, shift = c(-2500000, 100000))
#proj4string(Puerto_rico) = proj4string(us_map_aea)
slot(puerto_rico, "proj4string") <- slot(us_map_aea, "proj4string")

#guam
guam = us_map_aea[us_map_aea$STATEFP=="66",]
guam = elide(guam, shift = c(9000000, -6500000))
#proj4string(guam) = proj4string(us_map_aea)
slot(guam, "proj4string") <- slot(us_map_aea, "proj4string")

#virgin island
virgini<- us_map_aea[us_map_aea$STATEFP=="78",]
virgini = elide(virgini, shift = c(-2200000, 100000))
#proj4string(virgini) = proj4string(us_map_aea)
slot(virgini, "proj4string") <- slot(us_map_aea, "proj4string")

#us main continent and territories
us_aea<-rbind(us_main, alaska, hawaii, puerto_rico, guam, virgini)
plot(us_aea)



#small areas

#RI
rhode_island = us_map_aea[us_map_aea$STATEFP=="44",]
rhode_island = elide(rhode_island, shift=c(0,-200000))
#proj4string(rhode_island) = proj4string(us_map_aea)
slot(rhode_island, "proj4string") <- slot(us_map_aea, "proj4string")

#DE
delaware = us_map_aea[us_map_aea$STATEFP=="10",]
delaware = elide(delaware, shift=c(200000,0))
#proj4string(delaware) = proj4string(us_map_aea)
slot(delaware, "proj4string") <- slot(us_map_aea, "proj4string")

#DC
dc = us_map_aea[us_map_aea$STATEFP=="11",]
dc = elide(dc, scale=max(apply(bbox(dc), 1, diff)) / 1) # can't make smaller than 1.0
dc = elide(dc, shift=c(250000,-125000))
#proj4string(dc) = proj4string(us_map_aea)
slot(dc, "proj4string") <- slot(us_map_aea, "proj4string")

# map all
us_aea1 = rbind(us_aea, delaware, rhode_island, dc)
plot(us_aea1)

#save as a shapefile
library(sf)
#str(us_aea1)

#change the file path
#getwd()
writeOGR(us_aea1, "C:/Users/rvp8/OneDrive - CDC/Infoaid/", "us_territory", driver = "ESRI Shapefile")


###########map with labels
us50 <- fortify(us_aea1, region="STUSPS")
#us50 = remove.territories(us50)
centroids = data.frame(us_aea1$STUSPS, coordinates(us_aea1))
names(centroids) = c("id", "clong", "clat")

remove.territories = function(.df) {
  subset(.df, 
         .df$id != "AS" &
           .df$id != "MP" &
           .df$id != "GU" & 
           .df$id != "PR" &
           .df$id != "VI" 
  )
}

us50 <- fortify(us_aea1, region="STUSPS")
us50 = remove.territories(us50)

plain_theme = theme(axis.text=element_blank()) + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank())
no_ylab = ylab("") 
no_xlab = xlab("")

p = ggplot(data=us50) + 
  geom_map(map=us50, aes(x=long, y=lat, map_id=id, group=group), ,fill="white", color="black", size=0.3) + 
  no_ylab + 
  no_xlab + 
  plain_theme
p + geom_text(data=centroids, aes(clong, clat, label = id), size=2)


rownames(centroids) = centroids$id
# centroids['LA',]$clong = 729000
 centroids['FL',]$clong = 2200000
# centroids['DE',]$clong = 2400000
# centroids['RI',]$clong = 2400000
# centroids['MD',]$clong = 1950000
# centroids['MD',]$clat = -360000
# centroids['MA',]$clat = 100000
# centroids['NJ',]$clong = 2130000
# centroids['NJ',]$clat = -255000


c = p + geom_text(data=centroids, aes(clong, clat, label = id), size=2)
c
