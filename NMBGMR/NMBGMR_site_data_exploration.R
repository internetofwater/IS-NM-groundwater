
library(sp); library(rgdal); library(maptools); library(gplots); library(rgeos); library(raster)
library(stringr); library(PBSmapping); library(spData); library(sf); library(plyr); library(maps)
library(ggplot2); 
#tidyverse messes up maps function because purrr masks part of it

options(scipen=999) #turns scientific notation to numeric

NMBGMR.site <- read.csv("./NMBGMR/gwLevelMetadata.csv") #6090 sites

NMBGMR.site <- NMBGMR.site %>% dplyr::select(-countDepth2Water, -countManualD2W)
names(NMBGMR.site)


names(NMBGMR.site) <- c("SiteNo", "SiteID.NMBGMR", "Easting", 
                         "Northing", "AltVa",  "WellDepth",
                          "FormationZone.NMBGMR", "FormationMeaning.NMBGMR", 
                        "DepthToWater")

NMBGMR.site <- NMBGMR.site %>%
  mutate(AgencyCd="NMBGMR", AgencyNm = "New Mexico Bureau of Geology and Mineral Resources")
NMBGMR.site <- NMBGMR.site[!(is.na(NMBGMR.site$Easting)) | !(is.na(NMBGMR.site$Northing)),]

#create Lat/Long with Easting/Northing
utmcoor <- SpatialPoints(cbind(NMBGMR.site$Easting, NMBGMR.site$Northing), proj4string=CRS("+proj=utm +zone=13"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))

#Add lat/long into df
NMBGMR.site$DecLongVa <- coordinates(longlatcoor)[,1]
NMBGMR.site$DecLatVa <- coordinates(longlatcoor)[,2]

#separate from SpatialPoints chunk above 
NMBGMR.site.spatial <-  st_as_sf(NMBGMR.site, 
             coords = c("DecLongVa", "DecLatVa"), crs = 4326)
#NM coordinates 6528?, not what they used though


#map of NM counties
NM.county <- st_as_sf(map(database = "county",'new mexico', plot=TRUE, fill = TRUE, col = "white"))

#make sure projections are the same
st_crs(NM.county) #projection is 4326
st_crs(NMBGMR.site.spatial)


#add counties
intersect <- st_intersection(NMBGMR.site.spatial, NM.county)
intersect$CountyNm <- gsub("new mexico,", "", intersect$ID)
intersect$CountyNm <- stringr::str_to_title(intersect$CountyNm) %>%
                      paste0(" County") %>%
                      as.factor()

NMBGMR.site <- intersect %>% dplyr::select(-ID)


#save file as csv, keeping geometry column intact
write.csv(NMBGMR.site, file="./NMBGMR/NMBGMR.site.csv")


st_write(NMBGMR.site, "./NMBGMR/NMBGMR.site.csv", 
         layer_options = "GEOMETRY=AS_XY")


#------------------plotting-------------#

#plot well sites and levels over map of NM
NMBGMR.site.map <- ggplot() +
  geom_sf(data=NM.county, fill="white") +
  geom_sf(data = NMBGMR.site.spatial, aes(color = DepthToWater, fill=DepthToWater), 
          alpha = 0.5, size = 1)+
  labs(color = "Water level below ground surface", fill="Water level below ground surface")
  
print(NMBGMR.site.map)






