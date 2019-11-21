library(sf)
library(tidyverse)

getwd()
OSE <- read_sf("../OSE_Points_of_Diversion/OSE_Points_of_Diversion.shp")
names(OSE)
names(NGWMN_site_skinny)

OSE <- OSE %>% select(pod_nbr, county, elevation, depth_well, depth_wate, utm_zone, easting,
                      northing, datum, aquifer, city, state, zip, geometry, 
                      lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec,
                      start_date, log_file_d, sys_date)


