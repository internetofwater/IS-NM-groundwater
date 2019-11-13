
library(sp); library(rgdal); library(maptools); library(gplots); library(rgeos); library(raster)
library(stringr); library(PBSmapping); library(spData); library(sf); library(plyr); library(maps)
library(ggplot2); 
#tidyverse messes up maps function because purrr masks part of it


#read in both files that had data that was downloaded from the website on 11/4/19
allprojects <- read.csv("./NMBGMR/All Projects.csv")
collabwaterlevel <- read.csv("./NMBGMR/Collaborative Water Level Network.csv")
siteinfo <- read.csv("./")
##START HERE AND ADD IN MORE SITES
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

write.csv(NMBGMR.site, file="./NMBGMR/NMBGMR.site.csv")

NMBGMR.site.spatial <-  st_as_sf(NMBGMR.site, 
             coords = c("DecLongVa", "DecLatVa"), crs = 4326)
NM.county <- st_as_sf(map(database = "county",'new mexico', plot=TRUE, fill = TRUE, col = "white"))
st_crs(NM.county) #projection is 4326
st_crs(NMBGMR.site.spatial)
nrow(NM.county)
NM.county$num <- seq(1,nrow(NM.county),1)
NMBGMR.site.spatial$order <- seq(1,nrow(NMBGMR.site.spatial),1)

#attempting to grab counties and insert for each data point
class(NMBGMR.site.spatial)
test <- st_intersects(NMBGMR.site.spatial, NM.county)
test.table <- as.data.frame(test)

sapply(st_intersects(NM.county,NMBGMR.site.spatial), function(z) if (length(z)==0) NA_integer_ else z[1])
countynames <- sp::over(NM.county, NMBGMR.site.spatial)

#spatial merge Lauren's county solution
well.county <- st_join(NM.county, NMBGMR.site.spatial) 
well.county$ID <- str_remove_all(well.county$ID, "new mexico,")
well.county$CountyNm <- well.county$ID


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

ggplot() +
  geom_sf(data=well.county, fill="red")
