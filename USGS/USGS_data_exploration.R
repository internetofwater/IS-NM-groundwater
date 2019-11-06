library(dataRetrieval)
library(tidyverse)
library(sf)
library(maps)
library(readxl)

#re-do with different getNWIS function OR go through manually to see if you can't get more
#site info data

###GW level info
USGS.gwl <- readNWISdata(service = "gwlevels", stateCd= "NM", parameterCd = "72019")
#produced sites with lev_va (water level in feet below land surface)
USGS.gwl$site_no <- as.factor(USGS.gwl$site_no)
 #all observations have a date, very few (about 4000) have a time/timestamp
USGS.gwl$site_tp_cd <- as.factor(USGS.gwl$site_tp_cd)
summary(USGS.gwl$site_tp_cd)
#to simplify, get rid of all groundwater types except for wells
USGS.gwl <- USGS.gwl %>%
  filter(site_tp_cd == "GW")

 

#plot of all water levels
ggplot(USGS.gwl, (aes(x=site_no, y=lev_va))) +
  geom_point() +
  labs(x= "Site No.", y = "Water level below ground surface (ft)")


###site level info
USGS.sites <- whatNWISsites(stateCd="NM", service="gwlevels")
#produced site numbers with lat and long info
USGS.sites <- USGS.sites %>%
  filter(site_tp_cd == "GW") %>% #filter for just GW
  #link for more info on site type codes: https://help.waterdata.usgs.gov/code/site_tp_query?fmt=html
  select(agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, dec_long_va)

filepath <- "C:/Users/19524/Documents/DUKE/Independent Study - LP/IS-NM-groundwater/USGS/"
fileName <- "USGS_manual_data_collection.xlsx"

#following produces 8901 observations when pulling from USGS website
USGS.sites2 <- read_excel(paste0(filepath, fileName), sheet="Sheet2")
USGS.sites2$site_no <- as.character(USGS.sites2$site_no)

#combine both site collection methods to get fuller data frame with more variables
sites.combined <- 
  left_join(USGS.sites, USGS.sites2, 
            by=c("agency_cd", "site_no", "station_nm", "site_tp_cd", 
                 "dec_lat_va", "dec_long_va"))
  

#--------joining gwl with site-----------

#join site level dataset with gwl dataset
USGS.combined <- 
  left_join(USGS.gwl, sites.combined, by = c("agency_cd","site_no", "site_tp_cd"))

  

summary(USGS.combined$lev_va) 
#range of water levels is rather big...will be hard to visualize effectively

#create spatial dataset of combined df
USGS.combined.spatial <- st_as_sf(USGS.combined, 
                                     coords = c("dec_long_va", "dec_lat_va"), crs = 4326)

#create map of NM
states <- st_as_sf(map(database = "state", plot = TRUE, fill = TRUE, col = "white"))
NM.map <- states %>% filter(ID == "new mexico")
NMmap <- ggplot(NM.map) +
  geom_sf(fill="white")
print(NMmap)
st_crs(states) #projection is 4326, matches with spatial df

#plot well sites and levels over map of NM
USGS.combined.map <- ggplot() +
  geom_sf(data = NM.map, fill = "white") +
  geom_sf(data = USGS.combined.spatial, aes(color = lev_va), 
          alpha = 0.5, size = 1) +
  scale_color_viridis_c(direction=-1) +
  labs(color = "Water level below ground surface") +
  theme(legend.position = "top")
print(USGS.combined.map)

#---------##prep for joining USGS with other databases##-----------

#rename columns so that they match with NGWMN colnames

#sl_lev_va matched with "Water.level.in.feet.relative.to.NAVD.88 BUT we don't know the vertical datum (sl_datum_cd)


names(USGS.combined) <- 
  c("AgencyCd", "SiteNo", "SiteType", "Date", "Time", "lev_tz_cd_reported.USGS",
    "Depth.to.Water.Below.Land.Surface.in.ft.", 
    "Water.level.in.feet.relative.to.NAVD88", "sl_datum_cd", 
    "lev_status_cd.USGS", "Data.Provided.by", "lev_dt_acy_cd.USGS", "Accuracy.Value", 
    "lev_src_cd.USGS", "Observation.Method", "Comment", "DateTime", "lev_tz_cd.USGS", 
    "SiteName", "DecLatVa", "DecLongVa", "lat_va.USGS", "long_va.USGS", "HorzMethod", 
    "HorzAcy", "coord_datum_cd.USGS", "HorzDatum", "district_cd.USGS", "state_cd.USGS", 
    "CountyCd", "country_cd.USGS", "land_net_ds.USGS", "map_nm.USGS", "map_scale_fc.USGS", "AltVa",
    "AltMethod", "AltAcy", "AltDatumCd", "huc_cd.USGS", "basic_cd.USGS", "topo_cd.USGS", "construction_dt.USGS",
    "inventory_dt.USGS", "drain_area_va.USGS", "contrib_drain_area_va.USGS", "tz_cd.USGS", 
    "local_time_fg.USGS", "reliability_cd.USGS", "gw_file_cd.USGS", "NatAquiferCd", 
    "LocalAquiferCd", "aqfr_type_cd.USGS", "WellDepth", "hole_depth_va.USGS", "depth_src_cd.USGS",
    "project_no.USGS")
USGS.combined$Date <- as.Date(USGS.combined$Date)
USGS.combined$AltAcy <- as.factor(USGS.combined$AltAcy)


#all columns that relate to NGWMN have been renamed using the NGWMN column names.
#The columns that don't relate to NGWMN have been kept as is.


