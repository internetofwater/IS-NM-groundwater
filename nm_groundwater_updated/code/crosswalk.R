####################################################################################################################################################################
#
#  Creating crosswalk between groundwater datasets
#
####################################################################################################################################################################

#load librarys-------------------------------------------------------------------------------
library(tidyverse);  library (sf); library(sp); library(lubridate)
library(dataRetrieval); library(leaflet)

library(rgdal); library(maps);
####################################################################################################################################################################
#
#read in data--------------------------------------------------------------------------------
#
####################################################################################################################################################################
setwd("C:/Users/lap19/Documents/GitHub/nm_groundwater/")
options(scipen=999) #changes scientific notation to numeric (needed for usgs site_no)

#National groundwater monitoring portal----------------------------------------------------------------------------------------------------------------------------------------
portal.site <- read.csv("data/raw/national_portal/SITE_INFO.csv")
portal.gwl <- read.csv("data/raw/national_portal/WATERLEVEL.csv")

#USGS data - read in data from website----------------------------------------------------------------------------------------------------------------------------------------
###GW level info
usgs.gwl <- readNWISdata(service = "gwlevels", stateCd= "NM", parameterCd = "72019")
  table(usgs.gwl$agency_cd, useNA = "ifany")

###first finding all the site level numbers
usgs.site <- whatNWISsites(stateCd="NM", service="gwlevels")

#readNWIS sites can only pull a certain number at a time otherwise its too big, so split into two and remvoe usgs.sites with less than 8 characters because not correct site number
usgs.site <- subset(usgs.site, (nchar(as.character(site_no)) >= 8))
  #bind together for full metadata on site info
  USGS.site.top <- readNWISsite(usgs.site$site_no[1:30000])
  USGS.site.bottom <- readNWISsite(usgs.site$site_no[30001:dim(usgs.site)[1]])
usgs.site <- rbind(USGS.site.top, USGS.site.bottom)
#link for more info on site type codes: https://help.waterdata.usgs.gov/codes-and-parameters

#read in code tables
usgs.agencyCd <-read.delim("data/raw/usgs/agency_cd_query.txt") %>% mutate_all(as.character)
usgs.countyCd <-read.delim("data/raw/usgs/county_query.txt", as.is = TRUE) %>% filter(state_cd==35) %>% dplyr::select(county_cd, county_nm) %>% mutate_all(as.character)
  usgs.countyCd$county_cd <- as.character(usgs.countyCd$county_cd)
  usgs.countyCd$county_cd <- ifelse(nchar(usgs.countyCd$county_cd)==1, paste0("00", usgs.countyCd$county_cd), usgs.countyCd$county_cd)
  usgs.countyCd$county_cd <- ifelse(nchar(usgs.countyCd$county_cd)==2, paste0("0", usgs.countyCd$county_cd), usgs.countyCd$county_cd)
usgs.national <- read.delim("data/raw/usgs/nat_aqfr_query.txt") %>% filter(state_cd==35) %>% dplyr::select(nat_aqfr_cd, nat_aqfr_nm) %>% mutate_all(as.character)
usgs.local <- read.delim("data/raw/usgs/aqfr_cd_query.txt") %>% filter(state_cd==35) %>% dplyr::select(-state_cd) %>% mutate_all(as.character)
usgs.site.type <- read.delim("data/raw/usgs/site_tp_query.txt") %>% mutate_all(as.character)

#New Mexico Geology----------------------------------------------------------------------------------------------------------------------------------------
#site data
geology.site <- read.csv("data/raw/nm_geology/gwLevelMetadata.csv")
geology.gwl <- read.csv("data/raw/nm_geology/gwLevelData.csv")

#huc shapefile
huc.data <- readOGR("C:\\Users\\lap19\\Documents\\GIS\\HUCs", "NM_HUC8") 
  huc.data@data <- huc.data@data %>% dplyr::select(HUC8, NAME)
county <- readOGR("C:\\Users\\lap19\\Documents\\GIS\\USA", "CONUS_county")
  county <- county[as.character(county@data$STATEFP) =="35",] #subset to New Mexico
  county@data <- county@data %>% dplyr::select(COUNTYFP, GEOID, NAME)

  
#OSE DATA. SITE ONLY ----------------------------------------------------------------------------------------------------------------------------------------
#ose.site <- read.csv("data/raw/ose/GIS WATERS PODs.csv", sep = "\t") - could not figure out how to convert x,y to lat/long
ose.site <- read.csv("data/raw/ose/OSE_Points_of_Diversion.csv")
  
# Agreed upon site columns (remove horizontal datum, agency name, and aquifer codes, county codes, state stuff for faster web)
#AgencyCd	SiteNo	AgencyNm SiteName	DecLatVa	DecLongVa	HorzDatum	AltVa	AltDatumCd	StateNm StateCd CountyCd CountyNm	SiteType	WellDepth	
    #NatAquiferCd NationalAqfrName	LocalAquiferCd	LocalAquiferName	BasinCd BasinNm DataSource

#Agreed upon levels columns
#AgencyCd, SiteNo, Date, depthWaterFt

####################################################################################################################################################################
#
#clean data ---------
#
####################################################################################################################################################################
#create master data frame for sites and levels
all.sites <- as.data.frame(matrix(nrow=0, ncol=22)); 
  colnames(all.sites) <- c("AgencyCd", "AgencyNm", "SiteNo",	"SiteName",	"DecLatVa",	"DecLongVa",	"HorzDatum",	"AltVa",	"AltDatumCd", "WellDepth", "SiteType", "StateCd", "StateNm", 
                          "CountyCd", "CountyNm",	"NatAquiferCd", "NatAquiferName",	"LocalAquiferCd",	"LocalAquiferName",	"BasinCd", "BasinNm", "DataSource")

all.gwl <- as.data.frame(matrix(nrow=0, ncol=4));
  colnames(all.gwl) <- c("AgencyCd", "SiteNo", "Date", "DepthWaterFt")

# National Groundwater Monitoring POrtal data ------------------------------------------------------------------------------------------------------
#where does the portal get its data?
table(portal.site$AgencyCd, useNA = "ifany"); #all data domes from NMBGMR and USGS
#keep columns agreed upon
portal.site <- portal.site %>% dplyr::select(AgencyCd, AgencyNm, SiteNo, DecLatVa, DecLongVa, HorzDatum, AltVa, AltDatumCd, WellDepth, SiteType,
                                             StateCd, StateNm, CountyCd, CountyNm, NatAquiferCd, NatAqfrDesc, LocalAquiferCd, LocalAquiferName)
  names(portal.site)[names(portal.site) == 'NatAqfrDesc'] <- 'NatAquiferName'
#remove "County" from CountyNm field
portal.site$CountyNm <- substr(portal.site$CountyNm, 0, nchar(as.character(portal.site$CountyNm))-6)
  #table(portal.site$CountyNm)
#merge with National aquifer site because already removed aquifer and system to save space
portal.site <- merge(portal.site, usgs.national, by.x="NatAquiferCd", by.y="nat_aqfr_cd", all.x=TRUE)
portal.site$NatAquiferName <- portal.site$nat_aqfr_nm
portal.site <- portal.site %>% dplyr::select(-nat_aqfr_nm); #drop

#add Basin HUC8 and Basin Cd
huc.data <- st_as_sf(huc.data); huc.data <- st_transform(huc.data, 4269)
portal.site$DecLongVa2 <- portal.site$DecLongVa;    portal.site$DecLatVa2 <- portal.site$DecLatVa;
portal.site <-  st_as_sf(portal.site, coords = c("DecLongVa2", "DecLatVa2"), crs = 4269)
portal.site <- st_intersection(portal.site, huc.data); #takes a moment

#remove spatial component
st_geometry(portal.site) <- NULL
names(portal.site)[names(portal.site) == 'HUC8'] <- 'BasinCd'
names(portal.site)[names(portal.site) == 'NAME'] <- 'BasinNm'

portal.site <- portal.site %>% dplyr::select(AgencyCd, AgencyNm, SiteNo, DecLatVa, DecLongVa, HorzDatum, AltVa, AltDatumCd, WellDepth, SiteType,
                                             StateCd, StateNm, CountyCd, CountyNm, NatAquiferCd, NatAquiferName, LocalAquiferCd, LocalAquiferName, BasinCd, BasinNm)
portal.site$DataSource <- "NatGWPortal"

#convert to character for binding
portal.site <- portal.site %>%  mutate_all(as.character)

#rbind to main dataframe
all.sites <- bind_rows(all.sites, portal.site)

#create data field
portal.gwl$Date <- as.Date(as.character(portal.gwl$Time))
#for those with multiple readings in a single day - take the median value
portal.gwl <- portal.gwl %>% group_by(AgencyCd, SiteNo, Date) %>% summarise(DepthWaterFt = median(Depth.to.Water.Below.Land.Surface.in.ft., na.rm=TRUE), 
                                                                            DepthWaterNAVD88Ft = median(Water.level.in.feet.relative.to.NAVD88, na.rm=TRUE)) %>% as.data.frame()
#keep agreed upon columns
portal.gwl <- portal.gwl %>% dplyr::select(AgencyCd, SiteNo, Date, DepthWaterFt)
#make sure all gwl's have site data
unique.portal.site <- unique(portal.site$SiteNo)
portal.gwl <- portal.gwl[portal.gwl$SiteNo %in% unique.portal.site, ]

#rbind to main dataframe
all.gwl <- bind_rows(all.gwl, portal.gwl)
####################################################################################################################################################################



# USGS Data---------------------------------------------------------------------------------------------------------------------------------------------------
#sites
table(usgs.site$agency_cd, useNA="ifany"); #not all are from USGS
#find duplicates and keep last record
n_occur <- data.frame(table(usgs.site$site_no)) %>% filter(Freq > 1)
#zt <- usgs.site[usgs.site$site_no %in% n_occur$Var1,]
#keeps the last of duplicated sites - tends to be teh USGS version of a place    
usgs.site <- usgs.site[!rev(duplicated(rev(usgs.site$site_no))),]  

#Explore site type
table(usgs.site$site_tp_cd, useNA="ifany"); #Several non-groundwater well sites. Filter to just include GW sites
usgs.site <- usgs.site %>% filter(site_tp_cd == "GW") %>% as.data.frame()
usgs.site$site_tp_cd = "Well"; #can do since only version remaining

#add data
usgs.site <- merge(usgs.site, usgs.agencyCd, by.x="agency_cd", by.y="agency_cd", all.x=TRUE)
#add state
usgs.site$stateNm <- "New Mexico"
#add county
usgs.site <- merge(usgs.site, usgs.countyCd, by.x="county_cd", by.y="county_cd", all.x=TRUE)
#add source
usgs.site$DataSource <- "USGS"
#add hucname - they don't have a list of unique hucs I can find
#usgs.site <- merge(usgs.site, usgs.hucCd, by.x="huc_cd", by.y="HUC.Code", all.x=TRUE)
#  usgs.site$HUC.Name <- as.character(usgs.site$HUC.Name)
#add national name
usgs.site <- merge(usgs.site, usgs.national, by.x="nat_aqfr_cd", by.y="nat_aqfr_cd", all.x=TRUE)
#add local name
usgs.site <- merge(usgs.site, usgs.local, by.x="aqfr_cd", by.y="aqfr_cd", all.x=TRUE)

#70 do not have a basin code
usgs.site$DecLongVa2 <- usgs.site$DecLongVa;    usgs.site$DecLatVa2 <- usgs.site$DecLatVa;
usgs.site <-  st_as_sf(usgs.site, coords = c("DecLongVa2", "DecLatVa2"), crs = 4269)
usgs.site <- st_intersection(usgs.site, huc.data); #takes a moment

#remove spatial component
st_geometry(usgs.site) <- NULL
names(usgs.site)[names(usgs.site) == 'HUC8'] <- 'BasinCd'
names(usgs.site)[names(usgs.site) == 'NAME'] <- 'BasinNm'

#subset to agreed upon columns
usgs.site <- usgs.site %>% dplyr::select(agency_cd, party_nm, site_no, station_nm, dec_lat_va, dec_long_va, dec_coord_datum_cd, alt_va, alt_datum_cd, well_depth_va, site_tp_cd, state_cd, stateNm, county_cd,
                                         county_nm, nat_aqfr_cd, nat_aqfr_nm, aqfr_cd, aqfr_nm, BasinCd, BasinNm, DataSource)
#make sure all gwl's have site data
unique.usgs.site <- unique(usgs.site$site_no)
usgs.gwl <- usgs.gwl[usgs.gwl$site_no %in% unique.usgs.site, ]

colnames(usgs.site) <- c("AgencyCd", "AgencyNm", "SiteNo",	"SiteName",	"DecLatVa",	"DecLongVa",	"HorzDatum",	"AltVa",	"AltDatumCd", "WellDepth", "SiteType", "StateCd", "StateNm", 
                                       "CountyCd", "CountyNm",	"NatAquiferCd", "NatAquiferName",	"LocalAquiferCd",	"LocalAquiferName",	"BasinCd", "BasinNm", "DataSource")

#convert all columns to chracter for binding
usgs.site <- usgs.site %>% mutate_all(as.character)
#rbind to main dataframe
all.sites <- bind_rows(all.sites,usgs.site)

#remove duplicates based on site_no -47 duplicates are between portal and USGS
n_occur <- data.frame(table(all.sites$SiteNo)) %>% filter(Freq > 1)
all.sites <- all.sites[!rev(duplicated(rev(all.sites$SiteNo))),]  #kept last one because from USGS

#GW Levels
#convert gw date to date
usgs.gwl$lev_dt <- as.Date(usgs.gwl$lev_dt)
usgs.gwl$lev_va <- as.numeric(as.character(usgs.gwl$lev_va))

#keep only fields of interest
usgs.gwl <- usgs.gwl %>% dplyr::select(agency_cd, site_no, lev_dt, lev_va)
#for those with multiple readings in a day - what is 
usgs.gwl <- usgs.gwl %>% group_by(agency_cd, site_no, lev_dt) %>% summarise(DepthWaterFt = round(median(lev_va, na.rm=TRUE),2)) %>% as.data.frame()
  colnames(usgs.gwl) <- c("AgencyCd", "SiteNo", "Date", "DepthWaterFt")

#bind to main dataframe
all.gwl <- bind_rows(all.gwl, usgs.gwl)
#check and remove duplicate entries
zt <- all.gwl[duplicated(all.gwl),] #28 duplicates
#remove duplicates
all.gwl <- all.gwl[!duplicated(all.gwl),]
#remove those without a date or GW level
all.gwl <- all.gwl[complete.cases(all.gwl), ]
####################################################################################################################################################################


# NM Geology Data---------------------------------------------------------------------------------------------------------------------------------------------------
# Will do CKAN crosswalk in bit
#sites data
#remove sites without a location
geology.site <- geology.site %>% filter(is.na(easting)==FALSE & is.na(northing)==FALSE) %>% as.data.frame()
#create Lat/Long with Easting/Northing
utmcoor <- SpatialPoints(cbind(geology.site$easting, geology.site$northing), proj4string=CRS("+proj=utm +zone=13"))
  longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
#Add lat/long into df
geology.site$DecLongVa <- coordinates(longlatcoor)[,1]
geology.site$DecLatVa <- coordinates(longlatcoor)[,2]
#  plot(geology.site$DecLongVa, geology.site$DecLatVa, pch=19, cex=0.7)
#    points(portal.site$DecLongVa, portal.site$DecLatVa, pch=19, col="red") -- points overlap so in right coordinate system


#create columns
geology.site$StateCd <- "35";   geology.site$StateNm <- "New Mexico";

#make sure projections are the same
county <- st_as_sf(county)
county<- st_transform(county, 4269) 
huc.data <- st_as_sf(huc.data); huc.data <- st_transform(huc.data, 4269)
geology.site$DecLongVa2 <- geology.site$DecLongVa;    geology.site$DecLatVa2 <- geology.site$DecLatVa;
geology.site <-  st_as_sf(geology.site, coords = c("DecLongVa2", "DecLatVa2"), crs = 4269)
st_crs(county); st_crs(geology.site)

#plot
plot(st_geometry(county)); plot(st_geometry(geology.site), col="red", pch=19, cex=0.7, add=TRUE)
plot(st_geometry(huc.data)); plot(st_geometry(geology.site), col="red", pch=19, cex=0.7, add=TRUE)
geology.site <- st_intersection(geology.site, county)
geology.site <- st_intersection(geology.site, huc.data); #takes a moment

#remove spatial component
st_geometry(geology.site) <- NULL
geology.site$AgencyCd <- "NMBGMR"; geology.site$AgencyNm <- "New Mexico Bureau of Geological and Mineral Resources"
geology.site$HorzDatum <- "NAD83";
geology.site$DataSource <- "NMBGMR"

#subset to agreed upon columns
geology.site <- geology.site %>% dplyr::select(AgencyCd, AgencyNm, pointID, DecLatVa, DecLongVa, HorzDatum, altitude, well_depth, StateCd, StateNm, COUNTYFP, NAME, 
                                               formation_zone, formation_meaning, HUC8, DataSource)

colnames(geology.site) <- c("AgencyCd", "AgencyNm", "SiteNo",	"DecLatVa",	"DecLongVa",	"HorzDatum",	"AltVa", "WellDepth", "StateCd", "StateNm", 
                         "CountyCd", "CountyNm", "LocalAquiferCd",	"LocalAquiferName",	"BasinCd", "DataSource")

#make sure all gwl's have site data
unique.nm.site <- unique(geology.site$SiteNo)
geology.gwl <- geology.gwl[geology.gwl$pointID %in% unique.nm.site, ]; #removed some points

#convert all columns to chracter for binding
geology.site <- geology.site %>% mutate_all(as.character)
#rbind to main dataframe
all.sites <- bind_rows(all.sites,geology.site)

#remove duplicates based on site_no -11 duplicates are between portal and NM
n_occur <- data.frame(table(all.sites$SiteNo)) %>% filter(Freq > 1)
all.sites <- all.sites[!rev(duplicated(rev(all.sites$SiteNo))),]  #kept last one

#GW Levels----------------------------------------------------------------------------------------------
#convert gw date to date
geology.gwl$Date <- as.Date(geology.gwl$Date)
#remove na
geology.gwl <- geology.gwl %>% filter(is.na(Date) == FALSE) %>% as.data.frame()
#merge the water depth data
geology.gwl$DepthToWater <- ifelse(is.na(geology.gwl$MedDepth2WaterBGS)==TRUE, geology.gwl$MedManualDepth2WaterBGS, geology.gwl$MedDepth2WaterBGS)
geology.gwl$AgencyCd <- "NMBGMR"

#keep only fields of interest
geology.gwl <- geology.gwl %>% dplyr::select(AgencyCd, pointID, Date, DepthToWater)
colnames(geology.gwl) <- c("AgencyCd", "SiteNo", "Date", "DepthWaterFt")

#bind to main dataframe
all.gwl <- bind_rows(all.gwl, geology.gwl)
#check and remove duplicate entries
zt <- all.gwl[duplicated(all.gwl),] #254 duplicates
#remove duplicates
all.gwl <- all.gwl[!duplicated(all.gwl),]
####################################################################################################################################################################

# OSE SITE DATA
ose.site <- ose.site %>% dplyr::select(pod_file, depth_well, pod_status, status, use, state, finish_dat, depth_wate, 
                                       casing_siz, easting, northing, datum)
#remove those outside of NM and with NA for Dept.of.Well, casing.size, and Drilling Date
ose.site <- ose.site %>% filter(state == "NM");
ose.site <- ose.site %>% filter(is.na(easting)==FALSE & is.na(northing)==FALSE)
ose.site$finish_dat <- as.Date(as.character(substr(ose.site$finish_dat,1,10)))

#seems that well depth and depth to water are zero when NA
ose.site$depth_well <- ifelse(ose.site$depth_well==0, NA, ose.site$depth_well)
ose.site$depth_wate <- ifelse(ose.site$depth_wate==0, NA, ose.site$depth_wate)
ose.site$casing_siz <- ifelse(ose.site$casing_siz==0, NA, ose.site$casing_siz)

#assume if casing size is zero and finish date is zero and depth_wate is zero then it is not a well yet
ose.site <- ose.site %>% filter(is.na(depth_wate)==FALSE & is.na(casing_siz)==FALSE & is.na(depth_well)==FALSE) %>% as.data.frame()

#create gw.level.data and remove NA's for water depth
ose.gwl <- ose.site[,c("pod_file", "finish_dat", "depth_wate")] %>% filter(is.na(depth_wate)==FALSE) %>% as.data.frame()
#remove those missing a date or dated in the future
ose.gwl <- ose.gwl %>% filter(is.na(finish_dat)==FALSE & year(finish_dat) < 2020 & year(finish_dat) > 1900) %>% as.data.frame() 

ose.gwl$AgencyCd <- "OSE";
ose.gwl <- ose.gwl %>% dplyr::select(AgencyCd, pod_file, finish_dat, depth_wate)
  colnames(ose.gwl) <- c("AgencyCd", "SiteNo", "Date", "DepthWaterFt")

#bind to main dataframe... 
ose.site$State <- "New Mexico";   ose.site$StateCd <- "35";    ose.site$AgencyCd <- "OSE";    ose.site$AgencyNm <- "Office of State Engineers";

#create Lat/Long with Easting/Northing
utmcoor <- SpatialPoints(cbind(ose.site$easting, ose.site$northing), proj4string=CRS("+proj=utm +zone=13"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
#Add lat/long into df
ose.site$DecLongVa <- coordinates(longlatcoor)[,1]
ose.site$DecLatVa <- coordinates(longlatcoor)[,2]

#make sure projections are the same
ose.site$DecLongVa2 <- ose.site$DecLongVa;   ose.site$DecLatVa2 <- ose.site$DecLatVa;
ose.site <-  st_as_sf(ose.site, coords = c("DecLongVa2", "DecLatVa2"), crs = 4269)

#plot
plot(st_geometry(county)); plot(st_geometry(ose.site), col="red", pch=19, cex=0.7, add=TRUE)
#add county and huc info
ose.site <- st_intersection(ose.site, county); #takes a moment
ose.site <- st_intersection(ose.site, huc.data); #takes a moment

#remove spatial component
st_geometry(ose.site) <- NULL
ose.site$DataSource <- "OSE"

#subset to agreed upon columns
ose.site <- ose.site %>% dplyr::select(AgencyCd, AgencyNm, pod_file, DecLatVa, DecLongVa, datum, depth_well, use, StateCd, State, COUNTYFP, NAME, HUC8, DataSource)

colnames(ose.site) <- c("AgencyCd", "AgencyNm", "SiteNo",	"DecLatVa",	"DecLongVa",	"HorzDatum", "WellDepth", "SiteType", "StateCd", "StateNm", 
                            "CountyCd", "CountyNm", "BasinCd", "DataSource")

#convert all columns to chracter for binding
ose.site <- ose.site %>% mutate_all(as.character)
#rbind to main dataframe
all.sites <- bind_rows(all.sites, ose.site)

#bind to main dataframe
all.gwl <- bind_rows(all.gwl, ose.gwl)

#Remove some data to create space
rm(longlatcoor); rm(n_occur); rm(portal.gwl); rm(portal.site); rm(USGS.site.bottom); rm(USGS.site.top); rm(zt); rm(zt2); rm(unique.portal.site); rm(unique.usgs.site)



####################################################################################################################################################################
#
# SAVE DATA FOR WEBSITE DEVELOPMENT
#
####################################################################################################################################################################
#merge all.sites with basin to get name
st_geometry(huc.data) <- NULL
all.sites <- merge(all.sites, huc.data, by.x="BasinCd", by.y="HUC8", all.x=TRUE)
all.sites$BasinNm <- all.sites$NAME;

all.sites <- all.sites %>%  dplyr::select(AgencyCd, AgencyNm, SiteNo,	DecLatVa,	DecLongVa,	HorzDatum, WellDepth, SiteType, StateCd, StateNm, 
                        CountyCd, CountyNm, BasinCd, BasinNm, DataSource)

#strip extra space from county names to fix one error
all.sites$CountyNm <- trimws(all.sites$CountyNm, "both");
table(all.sites$CountyNm, useNA="ifany")

table(all.sites$BasinCd, useNA="ifany")
table(all.sites$BasinNm, useNA="ifany")


#save out all
write.csv(all.sites, "data/processed/all_sites.csv", row.names=FALSE)
write.csv(all.gwl, "data/processed/all_groundwater_levels.csv", row.names=FALSE)


# create summary stats for web legend
gwl.stats <- all.gwl %>% group_by(SiteNo) %>% summarise(startYr=min(year(Date)), endYr=max(year(Date)), gwlCount = n()) %>% as.data.frame()

#merge to site data
all.sites <- merge(all.sites, gwl.stats, by.x="SiteNo", by.y="SiteNo", all.x=TRUE)
all.sites$gwlCount <- ifelse(is.na(all.sites$gwlCount)==TRUE,0,all.sites$gwlCount)
all.sites$yearRange <- all.sites$endYr - all.sites$startYr

# save sites as those with more gwl data and those without any gwl data. Strip to bare minimum.
#round dec lat long to 6 digits
web.sites <- all.sites %>% dplyr::select(DataSource, SiteNo, DecLatVa, DecLongVa, WellDepth, CountyNm, BasinNm, startYr, endYr, gwlCount, yearRange)
  web.sites$DecLatVa <- round(as.numeric(as.character(web.sites$DecLatVa)),5);
  web.sites$DecLongVa <- round(as.numeric(as.character(web.sites$DecLongVa)),5); 
  web.sites$WellDepth <- as.numeric(as.character(web.sites$WellDepth))
  web.sites$BasinNm <- as.character(web.sites$BasinNm)


#big file so split up based on gwl counts
#sites.nogwl <- subset(web.sites, gwlCount==0);                   write.csv(sites.nogwl, "data/web/sites_nogwl.csv", row.names=FALSE)
#sites.onegwl <- subset(web.sites, gwlCount==1);                  write.csv(sites.onegwl, "data/web/sites_onegwl.csv", row.names=FALSE)
#sites.gwl <- subset(web.sites, gwlCount >= 1);                   write.csv(sites.gwl, "data/web/sites_gwl.csv", row.names=FALSE)

    
#save as spatial geojson
#make data spatial
web.sites.sp <- SpatialPointsDataFrame(cbind(web.sites$DecLongVa, web.sites$DecLatVa),data=web.sites,proj4string=CRS("+proj=longlat +datum=NAD83")) 

  #plot(web.sites.sp)
#web.sites.sp@data$Feature_Name<-as.character(web.sites.sp@data$SiteNo)
  #write to geojson
  writeOGR(web.sites.sp, "data/web/gw_sites.json", layer="web.sites.sp", driver="GeoJSON",check_exists=FALSE) 
  
test <- web.sites[1:1000,] %>% as.data.frame()
test <- SpatialPointsDataFrame(cbind(test$DecLongVa, test$DecLatVa), data=test)
#crs = 4269
writeOGR(test, "data/web/test_sites.json", layer="test", driver="GeoJSON",check_exists=FALSE) 

  
write.csv(web.sites, "data/web/sites_gwl.csv", row.names=FALSE)
# save gwl by datasource... if do that, can remove agency name field.

#histogram of unique wells by year
hist <- all.gwl %>% group_by(SiteNo, year(Date)) %>% summarize(count=n()) %>% as.data.frame()
colnames(hist) <- c("SiteNo","year","count")
hist2 <- hist %>% group_by(year) %>% summarize(count = n())%>% as.data.frame()
plot(hist2$year, hist2$count, pch=19, col=rgb(0,0,0,0.5), cex=0.7, ylab="Number Sites", xlab="", main="Number of Sites with Groundwater Level Measurement");
  lines(hist2$year, hist2$count, lwd=1, col="black")

web.gwl <- all.gwl %>% dplyr::select(-AgencyCd) %>% group_by(SiteNo) %>% arrange(Date) %>% as.data.frame()
write.csv(web.gwl, "data/web/gwl_data.csv", row.names=FALSE)

leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(~DecLongVa, ~DecLatVa,
                data = test,
                 stroke = FALSE, 
                 fillOpacity = 0.7,
                 radius=2,
                 fillColor = "blue",
                 layerId = ~SiteNo)







#Date is not in a good format so need to create
library(stringr)
zt2 <- str_split_fixed(ose.gwl$Drilling.Finish.Date, "/", 3) %>%  as.data.frame(); colnames(zt2) <- c("month", "day", "year")
zt2$month <- ifelse(nchar(as.character(zt2$month))==1, paste0("0", zt2$month), as.character(zt2$month));
zt2$day <- ifelse(nchar(as.character(zt2$day))==1, paste0("0", zt2$day), as.character(zt2$day));
zt2$Date <- as.Date(paste0(zt2$year,"-",zt2$month,"-", zt2$day))
ose.gwl$Date <- zt2$Date
