library(dataRetrieval)
library(tidyverse)
library(sf)
library(maps)


gwlsites <- readNWISdata(service = "gwlevels", stateCd= "NM", parameterCd = "72019")
#produced sites with lev_va (water level in feet below land surface)
summary(gwlsites$site_no)
gwlsites$site_no <- as.factor(gwlsites$site_no)
sum(is.na(gwlsites$lev_dt)) #all observations have a date, very few (about 4000 have a time/timestamp)
gwlsites$site_tp_cd <- as.factor(gwlsites$site_tp_cd)
summary(gwlsites$site_tp_cd)
#to simplify, get rid of all groundwater types except for wells
gwlsites <- gwlsites %>%
  filter(site_tp_cd == "GW")

ggplot(gwlsites, (aes(x=site_no, y=lev_va))) +
  geom_point()


readNWISpCode
NMsites <- whatNWISsites(stateCd="NM", service="gwlevels")
#produced site numbers with lat and long info
NMsites <- NMsites %>%
  filter(site_tp_cd == "GW") %>%
  select(agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, dec_long_va)

#create map of NM
states <- st_as_sf(map(database = "state", plot = TRUE, fill = TRUE, col = "white"))
NM.subset <- states %>% filter(ID == "new mexico")
NMmap <- ggplot(NM.subset) +
  geom_sf(fill="white")
print(NMmap)
#determine projection of NM
st_crs(states)

#create spatial dataset for NM sites
NMsites.spatial <- st_as_sf(NMsites, coords = c("dec_long_va", "dec_lat_va"), crs = 4326)

#plot sites over NM map
NMsitesmap <- ggplot() +
  geom_sf(data = NM.subset, fill = "white") +
  geom_sf(data = NMsites.spatial, #aes(color = ), 
          alpha = 0.5, size = 1) +
  scale_color_viridis_c() +
  #labs(color = "") +
  theme(legend.position = "top")
print(NMsitesmap)
