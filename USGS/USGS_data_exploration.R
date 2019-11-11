library(dataRetrieval)
library(tidyverse)
library(sf)
library(maps)
library(readxl)
library(plyr)

#re-do with different getNWIS function OR go through manually to see if you can't get more
#site info data

###GW level info
USGS.gwl <- readNWISdata(service = "gwlevels", stateCd= "NM", parameterCd = "72019")
#produced sites with lev_va (water level in feet below land surface)
USGS.gwl$site_no <- as.factor(USGS.gwl$site_no)
 #all observations have a date, very few (about 4000) have a time/timestamp
USGS.gwl$site_tp_cd <- as.factor(USGS.gwl$site_tp_cd)
summary(USGS.gwl$site_tp_cd)
USGS.gwl$lev_dt <- as.Date(USGS.gwl$lev_dt)


###site level info
USGS.site <- whatNWISsites(stateCd="NM", service="gwlevels")
#produced site numbers with lat and long info
USGS.sitenumbers <- USGS.site %>%
  select(agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, dec_long_va)

USGS.sitenumbers.top <- USGS.sitenumbers %>% top_n(30000)
USGS.sitenumbers.top <- USGS.sitenumbers.top$site_no
USGS.sitenumbers.bottom <- USGS.sitenumbers %>%top_n(-8339)
USGS.sitenumbers.bottom <- USGS.sitenumbers.bottom$site_no

USGS.site.top <- readNWISsite(USGS.sitenumbers.top)
USGS.site.bottom <- readNWISsite(USGS.sitenumbers.bottom)
USGS.site <- rbind(USGS.site.top, USGS.site.bottom)

#link for more info on site type codes: https://help.waterdata.usgs.gov/code/site_tp_query?fmt=html


USGS.site$site_no <- as.factor(USGS.site$site_no)
USGS.site <- USGS.site %>%
  mutate(AgencyNm = "U.S. Geological Survey")
length(unique(USGS.site$site_no))
USGS.site <- unique(USGS.site)

str(USGS.site)

 
#--------to plot-----------#

#plot of all water levels
ggplot(USGS.gwl, (aes(x=site_no, y=lev_va))) +
  geom_point() +
  labs(x= "Site No.", y = "Water level below ground surface (ft)")


#join site level dataset with gwl dataset
USGS.combined <- 
  left_join(USGS.gwl, USGS.site, by = c("agency_cd","site_no", "site_tp_cd"))

summary(USGS.combined$lev_va) 
#range of water levels is rather big...will be hard to visualize effectively

#create spatial dataset of combined df
USGS.combined.spatial <- st_as_sf(USGS.combined, 
                                     coords = c("dec_long_va", "dec_lat_va"), crs = 4326)

#create map of NM
states <- st_as_sf(map(database = "state", plot=TRUE, fill = TRUE, col = "white"))
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


#sl_lev_va matched with "Water.level.in.feet.relative.to.NAVD.88 BUT we don't know the vertical datum (sl_datum_cd)



