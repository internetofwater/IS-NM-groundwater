
library(sp); library(rgdal); library(maptools); library(gplots); library(rgeos); library(raster)
library(stringr); library(PBSmapping); library(spData); library(sf); library(plyr); library(maps)
library(ggplot2); 
#tidyverse messes up maps function because purrr masks part of it



NMBGMR.site <- read.csv("./NMBGMR/NMBGMR_SiteInfo.csv") #6091 sites
names(NMBGMR.site)

#do not know the coordinate reference system they used

names(NMBGMR.site) <- c("SiteNo", "SiteID.NMBGMR", "Easting", 
                         "Northing", "AltVa", "OSEWellID", "HoleDepth",
                         "WellDepth", "FormationZone.NMBGMR", "Geometry", 
                        "ObjectID.NMBGMR","DecLongVa", "DecLatVa", "wkid")

NMBGMR.site <- NMBGMR.site %>%
  dplyr::select(-ObjectID.NMBGMR, -Geometry, -wkid) %>%
  mutate(AgencyCd="NMBGMR", AgencyNm = "New Mexico Bureau of Geology and Mineral Resources")





NMBGMR.site.spatial <-  st_as_sf(NMBGMR.site, 
             coords = c("DecLongVa", "DecLatVa"), crs = 4326)

#map of NM counties
NM.county <- st_as_sf(map(database = "county",'new mexico', plot=TRUE, fill = TRUE, col = "white"))

#make sure projections are the same
st_crs(NM.county) #projection is 4326
st_crs(NMBGMR.site.spatial)

intersect <- st_intersection(NMBGMR.site.spatial, NM.county)
intersect$CountyNm <- gsub("new mexico,", "", intersect$ID)
intersect$CountyNm <- stringr::str_to_title(intersect$CountyNm) %>%
                      paste0(" County") %>%
                      as.factor()

NMBGMR.site <- intersect %>% select(-ID)

write.csv(NMBGMR.site, file="./NMBGMR/NMBGMR.site.csv")


#------------------plotting-------------#

#plot well sites and levels over map of NM
NMBGMR.site.map <- ggplot() +
  geom_sf(data=NM.county, fill="white") +
  geom_sf(data = test, aes(color = ID, fill=ID), 
          alpha = 0.5, size = 1)
  
print(NMBGMR.site.map)


