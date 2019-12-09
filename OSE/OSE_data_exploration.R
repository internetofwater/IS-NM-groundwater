library(sf)
library(tidyverse)
library(sp)
library(rgdal)
library(maps)

getwd()

OSE <- st_read("../OSE_Points_of_Diversion/OSE_Points_of_Diversion.shp")
names(OSE)

#pull out important columns
OSE <- OSE %>% select(OBJECTID, pod_basin, pod_nbr, pod_suffix, pod_name, county, elevation, depth_well, 
                      depth_wate, use_of_wel, utm_zone, easting, cs_code,
                      northing, datum, aquifer, city, state, zip,  
                      lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec,
                      lat_lon_ac, start_date, log_file_d, sys_date, static_lev, casing_siz,
                      geometry)

names(OSE) <- c("OBJECTID.OSE", "pod_basin.OSE", "OSEWellID", "pod_suffix.OSE", "site_nm",
                "CountyNm", "AltVa", "WellDepth", "depth_water.OSE","use_of_well.OSE",
                "UTM_zone", "Easting", "crs_code", "Northing", "HorzDatum", "Aquifer", 
                "City", "State", "Zip", "lat_deg", "lat_min", "lat_sec",
                "lon_deg", "lon_min", "lon_sec", "lat_lon_ac", "start_date.OSE", 
                "log_file_date", "sys_date", "static_lev", "Casing_sz", "geometry")


OSE$AltVa[OSE$AltVa==0] <- NA
OSE$Casing_sz[OSE$Casing_sz==0] <- NA
OSE$WellDepth[OSE$WellDepth==0] <- NA

#fix county name
levels(OSE$CountyNm) <- c("Bernalillo County", "Catron County", "Chaves County","Cibola County",
                        "Colfax County","Curry County","Dona Ana County","De Baca County",
                        "Eddy County","Grant County","Guadalupe County","Harding County",
                        "Hidalgo County","Los Alamos County","Lea County","Lincoln County",
                        "Luna County","Mckinley County","Mora County","Otero County","Quay County",
                        "Rio Arriba County","Roosevelt County","Sandoval County","Santa Fe County",
                        "Sierra County","San Juan County","San Miguel County","Socorro County",
                        "Taos County","Torrance County","Union County","Valencia County","XM", "XX")

OSE$AgencyCd <- "OSE"
OSE$AgencyNm <- "Office of the State Engineer"

#create site and gwl dataframes
OSE.site <- OSE %>% select(AgencyCd, AgencyNm, OBJECTID.OSE, pod_basin.OSE, OSEWellID, pod_suffix.OSE,
                           site_nm, AltVa, UTM_zone, Easting, Northing, crs_code, HorzDatum,
                           State, CountyNm, City, Zip, Aquifer, WellDepth, depth_water.OSE,
                           use_of_well.OSE, Casing_sz, geometry)
OSE.gwl <- OSE %>% select(AgencyCd, OSEWellID, sys_date, static_lev) %>% as.data.frame()

st_crs(OSE.site)
OSE.site <- st_transform(OSE.site,4269)


#fix Date and Time 
OSE.gwl$Date <- as.Date(OSE.gwl$sys_date)
OSE.gwl$Time <- substring(OSE.gwl$sys_date, 12, 19)
OSE.gwl$Time <- hms::as_hms(OSE.gwl$Time)
OSE.gwl <- OSE.gwl %>% select(-geometry)

st_write(OSE.site, "./OSE/OSE.site.csv", 
         layer_options = "GEOMETRY=AS_XY")
write.csv(OSE.gwl, "./OSE/OSE.gwl.csv")


#-----------plotting--------------------#
proj<- st_crs(OSE)
NM.county <- st_as_sf(map(database = "county",'new mexico', plot=TRUE, fill = TRUE, col = "white"))

#make sure projections are the same
st_crs(NM.county) #projection is 4326
st_set_crs(NM.county, proj) #set proj to 26913 to match OSE

OSE.site.map <- ggplot() +
  geom_sf(data=NM.county, fill="white") +
  geom_sf(data = OSE, aes(color = depth_water.OSE, fill=depth_water.OSE), 
          alpha = 0.5, size = 1)+
  labs(color = "Water level below ground surface", fill="Water level below ground surface")
print(OSE.site.map)
