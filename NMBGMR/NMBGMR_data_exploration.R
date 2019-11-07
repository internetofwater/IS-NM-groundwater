pacman::p_load(tidyverse, sf, maps, plyr, readxl)

#read in both files that had data that was downloaded from the website
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
  select(-ObjectID.NMBGMR) %>%
  mutate(AgencyCd="NMBGMR", AgencyNm = "New Mexico Bureau of Geology and Mineral Resources")
