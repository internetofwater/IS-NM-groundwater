pacman::p_load(sf, maps, plyr, readxl, tidyverse)
#library(sp); library(rgdal); library(maptools); library(gplots); library(rgeos); library(raster)
#library(stringr); library(PBSmapping); library(spData); library(sf)



#read in both files that had data that was downloaded from the website on 11/4/19
allprojects <- read.csv("./NMBGMR/All Projects.csv")
collabwaterlevel <- read.csv("./NMBGMR/Collaborative Water Level Network.csv")
str(collabwaterlevel) #evaluate the structure
names(allprojects)
names(collabwaterlevel)
NMBGMR.site <- rbind.fill(allprojects, collabwaterlevel) #rbind both dfs
length(unique(NMBGMR.site$ï..PointID)) #there are some point IDs that overlap...not sure what to do about that

#do not know the coordinate reference system they used

names(NMBGMR.site) <- c("SiteNo", "SiteID.NMBGMR", "Easting", 
                         "Northing", "AltVa", "OSEWellID", "HoleDepth",
                         "WellDepth", "FormationZone.NMBGMR", "ObjectID.NMBGMR",
                         "DecLongVa", "DecLatVa", "ProjectName.NMBGMR", 
                         "MonitoringStatus.NMBGMR")

NMBGMR.site <- NMBGMR.site %>%
  dplyr::select(-ObjectID.NMBGMR) %>%
  mutate(AgencyCd="NMBGMR", AgencyNm = "New Mexico Bureau of Geology and Mineral Resources")


NMBGMR.site.spatial <-  st_as_sf(NMBGMR.site, 
             coords = c("DecLongVa", "DecLatVa"), crs = 4326)
NM.county <- st_as_sf(map(database = "county",'new mexico', plot=TRUE, fill = TRUE, col = "white"))
st_crs(NM.county) #projection is 4326, matches with spatial df



#spatial merge
well.county <- st_join(NM.county, NMBGMR.site.spatial, left=TRUE) 

#finish by renamin ID name and getting rid of "new mexico" from id name and mapping

#plot well sites and levels over map of NM
NMBGMR.site.map <- ggplot() +
  geom_sf(data = NM.county, fill = "white") +
  geom_sf(data = NMBGMR.site.spatial, aes(color = WellDepth), 
          alpha = 0.5, size = 1) +
  scale_color_viridis_c(direction=-1) +
  labs(color = "Water Depth") +
  theme(legend.position = "top")
print(NMBGMR.site.map)