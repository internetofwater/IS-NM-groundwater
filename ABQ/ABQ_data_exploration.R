library(tidyverse)
library(sf)
library(readxl)
library(maps)
library(sp)
library(rgdal)

# read in site data
path <- "C:/Users/19524/Documents/DUKE/Independent Study - LP/IS-NM-groundwater/ABQ/"
fileName <- "CityABQ_DT_Location_Example_Dictionary.xlsx"

ABQ.site <- read_excel(paste0(path, fileName), sheet="ExampleEntry")

names(ABQ.site)

ABQ.site <- ABQ.site %>% select(sys_loc_code, loc_name, facility_id, loc_major_basin,
                                loc_county_code, )

# read in gwl data
filename2 <- "CityABQ_DT_Water_Level_Data.xlsx"
ABQ.gwl <- read_excel(paste0(path, filename2), sheet = "Sheet2")

names(ABQ.gwl)
ABQ.gwl <- ABQ.gwl %>% select(facility_id, sys_loc_code, measurement_date, water_level_depth,
                              water_level_elev, measurement_method, measured_depth_of_well,
                              dry_indicator_yn, technician)
ABQ.gwl <- ABQ.gwl %>% mutate(AgencyCd="")
data <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","ABQ")
ABQ.gwl[1,] <- data

#save csv's
write.csv(ABQ.gwl, "./ABQ/ABQ.gwl.csv")
write.csv(ABQ.site, "./ABQ/ABQ.site.csv")

