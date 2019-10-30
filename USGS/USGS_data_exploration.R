library(dataRetrieval)
library(tidyverse)
library(sf)
library(maps)


###GW level info
USGS.gwl <- readNWISdata(service = "gwlevels", stateCd= "NM", parameterCd = "72019")
#produced sites with lev_va (water level in feet below land surface)
summary(USGS.gwl$site_no)
USGS.gwl$site_no <- as.factor(USGS.gwl$site_no)
sum(is.na(USGS.gwl$lev_dt)) #all observations have a date, very few (about 4000) have a time/timestamp
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
  select(agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, dec_long_va)


#join site level dataset with gwl dataset
USGS.combined <- 
  left_join(gwl, NMsites, by = c("agency_cd","site_no", "site_tp_cd")) #%>%
  #filter(lev_va < 2000) #filtered only so that it can be better mapped, shouldn't keep this filter

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

#---------##prep for joining##-----------

#rename columns so that they match with NGWMN colnames
#need to convert lev_dt, lev_tm, lev_tz_cd_reported and combine for UTC time (many don't have time of day values)
#sl_lev_va could match with "Water.level.in.feet.relative.to.NAVD.88 BUT we don't know the vertical datum (sl_datum_cd)
#no equivalent to lev_status_cd, lev_dt_acy_cd, lev_src_cd, lev_age_cd,
#not necessary: lev_tz_cd, colocated, queryTime,
names(USGS.combined.spatial) <- 
  c("AgencyCd", "SiteNo", "SiteType", "lev_dt", "lev_tm", "lev_tz_cd_reported",
    "Depth.to.Water.Below.Land.Surface.in.ft", "sl_lev_va", "sl_datum_cd", 
    "lev_status_cd", "Data.Provided.by", "lev_dt_acy_cd", "Accuracy.Value", 
    "lev_src_cd", "Observation.Method", "lev_age_cd", "Time", "lev_tz_cd", 
    "SiteName", "colocated", "queryTime", "geometry")

