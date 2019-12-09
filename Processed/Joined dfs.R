pacman::p_load(tidyverse, plyr, dataRetrieval, devtools)



#--------joining gwl dfs-----------#

#NGWMN = NGWMN.gwl.skinny and USGS = USGS.gwl

NGWMN.gwls <- read.csv("./NGWMN/NGWMN.gwl.csv")
USGS.gwls <- read.csv("./USGS/USGS.gwl.csv")
NMBGMR.gwls <- read.csv("./NMBGMR/NMBGMR.gwl.csv")
OSE.gwls <- read.csv("./OSE/OSE.gwl.csv")
ABQ.gwls <- read.csv("./ABQ/ABQ.gwl.csv")


#rename so that all overlapping variables with NGWMN have the same name 

names(USGS.gwls)
names(NGWMN.gwls) <- c("X", "AgencyCd", "SiteNo", "DateTime", "OriginalParameter",
                       "OriginalDirection", "OriginalUnit", "OriginalValue", "AccuracyUnit",
                       "AccuracyValue", "DepthToWater", "WaterRelativeToNAVD88", 
                       "Comment", "ObservationMethod", "DataProvidedBy", "Date", "Time")

names(USGS.gwls) <- c("X","AgencyCd", "SiteNo", "site_tp_cd.USGS", "Date", "Time", 
                     "lev_tz_cd_reported.USGS", "DepthToWater",
                     "WaterRelativeToNAVD88", "sl_datum_cd.USGS", 
                     "Status", "DataProvidedBy", "lev_dt_acy_cd.USGS",
                     "AccuracyValue", "lev_src_cd.USGS", "ObservationMethod", 
                     "Comment", "DateTime", "lev_tz_cd.USGS")

names(NMBGMR.gwls)<- c("X", "SiteNo", "Date", "Status", "ObservationMethod", "MedDepth2WaterBGS.NMBGMR",
                       "MedManualDepth2WaterBGS.NMBGMR", "DepthToWater", "AgencyCd")

names(OSE.gwls)<- c("X","AgencyCd", "OSEWellID", "sys_date.OSE", "DepthToWater", "Date", "Time")

names(ABQ.gwls) <- c("X", "facility_id.ABQ", "SiteNo", "Date", "DepthToWater", "water_level_elev",
                     "ObservationMethod", "WellDepth", "Status", "Technician", "WaterRelativeToNAVD88",
                     "AgencyCd")


gwl.joined <- rbind.fill(NGWMN.gwls, USGS.gwls)
gwl.joined <- rbind.fill(gwl.joined, NMBGMR.gwls)
gwl.joined <- rbind.fill(gwl.joined, OSE.gwls)
gwl.joined <- rbind.fill(gwl.joined, ABQ.gwls)
gwl.joined.skinny <- gwl.joined %>%
  select(AgencyCd, SiteNo, OSEWellID, Date, Time, DepthToWater, AccuracyValue, 
         WaterRelativeToNAVD88, ObservationMethod, Status)

saveRDS(gwl.joined, "./Processed/gwl.joined.rds")
saveRDS(gwl.joined.skinny, "./Processed/gwl.joined.skinny.rds")

#write.csv(gwl.joined, file = "./Processed/gwl.joined.csv")
#write.csv(gwl.joined.skinny, file="./Processed/gwl.joined.skinny.csv")


#--------joining site info dfs-----------#

#USGS = sites.combined, NGWMN = NGWMN.site.skinny, NMBGMR = NMBGMR.site
USGS.sites <- read.csv("./USGS/USGS.site.csv")
USGS.sites <- USGS.sites %>% select(-X)
NGWMN.sites <- read.csv("./NGWMN/NGWMN.site.skinny.csv") 
NGWMN.sites <- NGWMN.sites %>% select(-X)
#add in correct meta data for NMBGMR sites


str(USGS.sites)
str(NGWMN.sites)                         

names(NGWMN.sites)
names(USGS.sites)


names(USGS.sites) <- 
  c("AgencyCd", "SiteNo", "SiteName", "SiteType", "lat_va.USGS", "long_va.USGS", 
    "DecLatVa", "DecLongVa", "HorzMethod", "HorzAcy", "coord_datum_cd.USGS",
    "HorzDatum", "district_cd.USGS", "StateCd", "CountyCd", "country_cd.USGS", 
    "land_net_ds.USGS", "map_nm.USGS", "map_scale_fc.USGS", "AltVa", "AltMethod", 
    "AltAcy", "AltDatumCd", "huc_cd.USGS", "basic_cd.USGS", "topo_cd.USGS", 
    "instruments_cd.USGS", 
    "construction_dt.USGS", "inventory_dt.USGS", "drain_area_va.USGS", 
    "contrib_drain_area_va.USGS", "tz_cd.USGS", "local_time_fg.USGS", 
    "reliability_cd.USGS", "gw_file_cd.USGS", "NatAquiferCd", "LocalAquiferCd", 
    "aqfr_type_cd.USGS", "WellDepth", "HoleDepth", "depth_src_cd.USGS",
    "project_no.USGS", "AgencyNm")

sites.joined <- rbind.fill(NGWMN.sites, USGS.sites)
sites.joined$OSEWellID <- ""

sites.joined <- rbind.fill(sites.joined, NMBGMR.sites)
names(sites.joined)

sites.joined.skinny <- sites.joined %>%
  select(AgencyCd, SiteNo, AgencyNm, SiteName, OSEWellID, DecLatVa, DecLongVa, 
         HorzDatum, AltVa, AltDatumCd, CountyNm, SiteType, WellDepth, LocalAquiferName)


write.csv(sites.joined, file = "./Processed/sites.joined.csv")
write.csv(sites.joined.skinny, file="./Processed/sites.joined.skinny.csv")



#Join of both gwl and meta data....probably too big.
gwl.sites.joined <- left_join(gwl.joined.skinny, sites.joined.skinny, 
                              by = c("SiteNo", "AgencyCd"))

write.csv(gwl.sites.joined, file = "./Processed/gwl.sites.joined.csv")


#--------create static gwl summarized df-----------#
gwl.joined.skinny$Date <- as.Date(gwl.joined.skinny$Date)
gwl.joined.skinny.static <- gwl.joined.skinny %>%
  dplyr::group_by(AgencyCd, SiteNo) %>%
  dplyr::summarise(
    firstMeas = min(Date),
    lastMeas = max(Date), 
    Count = length(SiteNo)
  )

sites.summary.static <- left_join(sites.joined.skinny, gwl.joined.skinny.static, by = "SiteNo")
write.csv(sites.summary.static, file = "./Processed/sites.summary.static.csv")
