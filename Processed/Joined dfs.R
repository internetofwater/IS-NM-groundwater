pacman::p_load(tidyverse, plyr, dataRetrieval, devtools)
library(data.table)

options(scipen=999)

#--------joining gwl dfs-----------#

#NGWMN = NGWMN.gwl.skinny and USGS = USGS.gwl

NGWMN.gwls <- read.csv("./NGWMN/NGWMN.gwl.csv")
USGS.gwls <- read.csv("./USGS/USGS.gwl.csv")
NMBGMR.gwls <- read.csv("./NMBGMR/NMBGMR.gwl.csv")
OSE.gwls <- read.csv("./OSE/OSE.gwl.csv")
ABQ.gwls <- read.csv("./ABQ/ABQ.gwl.csv") #no groundwater level measurements


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

fwrite(gwl.joined.skinny, "./Processed/gwl.joined.skinny.csv")

#write.csv(gwl.joined, file = "./Processed/gwl.joined.csv")
write.csv(gwl.joined.skinny, file="./Processed/gwl.joined.skinny.csv")


#--------joining site info dfs-----------#


USGS.sites <- read.csv("./USGS/USGS.site.csv")
NGWMN.sites <- read.csv("./NGWMN/NGWMN.site.skinny.csv") 
NMBGMR.sites <- read.csv("./NMBGMR/NMBGMR.site.csv")
ABQ.sites <- read.csv("./ABQ/ABQ.site.csv")
OSE.sites <- read.csv("./OSE/OSE.site.csv")


names(NGWMN.sites)


names(USGS.sites) <- 
  c("X", "AgencyCd", "SiteNo", "SiteName", "SiteType", "lat_va.USGS", "long_va.USGS", 
    "DecLatVa", "DecLongVa", "HorzMethod", "HorzAcy", "coord_datum_cd.USGS",
    "HorzDatum", "DistrictCd", "StateCd", "CountyCd", "CountryCd", 
    "land_net_ds.USGS", "map_nm.USGS", "map_scale_fc.USGS", "AltVa", "AltMethod", 
    "AltAcy", "AltDatumCd", "huc_cd.USGS", "BasinCd", "topo_cd.USGS", 
    "instruments_cd.USGS", 
    "construction_dt.USGS", "inventory_dt.USGS", "drain_area_va.USGS", 
    "contrib_drain_area_va.USGS", "tz_cd.USGS", "local_time_fg.USGS", 
    "reliability_cd.USGS", "gw_file_cd.USGS", "NatAquiferCd", "LocalAquiferCd", 
    "aqfr_type_cd.USGS", "WellDepth", "HoleDepth", "depth_src_cd.USGS",
    "project_no.USGS", "AgencyNm", "CountyNm")

names(NMBGMR.sites) <- c("DecLongVa", "DecLatVa", "SiteNo", "SiteID.NMBGMR", "Easting",
                         "Northing", "AltVa", "WellDepth", "FormationZone.NMBGMR",
                         "FormationMeaning.NMBGMR", "DepthToWater", "AgencyCd",
                         "AgencyNm", "CountyNm")

names(ABQ.sites) <- c("X", "facility_id.ABQ", "SiteNo", "SiteName", "data_provider",
                      "subfacility_code.ABQ", "loc_desc.ABQ","loc_type.ABQ","loc_purpose.ABQ",
                      "loc_type_2.ABQ","BasinCd","within_facility_yn.ABQ","CountyNm",
                      "DistrictCd","StateCd", "loc_minor_basin.ABQ","custome_field_1.ABQ",
                      "stream_code.ABQ","custom_field_2.ABQ","stream_mile.ABQ",
                      "custom_field_3.ABQ","custom_field_4.ABQ","phase_code.ABQ",
                      "custom_field_5.ABQ","remark_1.ABQ","bore_id.ABQ","remark_2.ABQ",
                      "StartDate","EndDate","DrillingMethod", "geologist.ABQ",
                      "sampling_method.ABQ","drawing_checker.ABQ","drawing_check_date.ABQ",
                      "drawing_editor.ABQ","drawing_edit_date.ABQ","driller.ABQ",
                      "units.ABQ","depth_to_bedrock.ABQ","log_date.ABQ","WellDepth",
                      "bearing.ABQ","Comment","plunge.ABQ","drilling_subcontractor.ABQ",
                      "engineer_subcontractor.ABQ","engineer.ABQ","estab_company_code.ABQ",
                      "excav_company_code.ABQ","inspector.ABQ","inspect_subcontractor.ABQ",
                      "ebatch.ABQ","map_code.ABQ","parent_loc_code.ABQ","status_flag.ABQ",
                      "TimeZone", "euid.ABQ","land_use.ABQ","hole_diameter.ABQ",
                      "hole_diameter_unit.ABQ","alert_purge_criteria.ABQ","offset.ABQ",
                      "station.ABQ","route.ABQ", "AgencyCd")

names(OSE.sites) <- c("DecLongVa", "DecLatVa", "AgencyCd", "AgencyNm","OBJECTID.OSE",
                      "pod_basin.OSE", "OSEWellID", "pod_suffix.OSE", "SiteName",
                      "AltVa", "TimeZone", "Easting", "Northing", "crs_code", "HorzDatum",
                      "StateCd", "CountyNm", "City", "Zip", "LocalAquiferName",
                      "WellDepth", "depth_water.OSE", "use_of_well.OSE", "CasingSize")


############################### DEAL WITH DUPLICATED SITE NUMBERS #######################################################################################################
#length unique ids
dim(USGS.sites)[1]; length(unique(USGS.sites$SiteNo))  #30 duplicates
#find duplicates and keep last record
n_occur <- data.frame(table(USGS.sites$SiteNo))
#zt <- n_occur[n_occur$Freq > 1,]
zt <- USGS.sites[USGS.sites$SiteNo %in% n_occur$Var1[n_occur$Freq > 1],]
#keeps the last of duplicated sites    
USGS.sites <- USGS.sites[!rev(duplicated(rev(USGS.sites$SiteNo))),]  
summary(USGS.sites$AgencyCd)

#NGWMN.sites
dim(NGWMN.sites)[1]; length(unique(NGWMN.sites$SiteNo)) #no duplicates
summary(NGWMN.sites$AgencyCd)

#NMBGMR.sites
dim(NMBGMR.sites)[1];   length(unique(NMBGMR.sites$SiteNo)) # no duplicates
summary(NMBGMR.sites$AgencyCd)

#OSE.sites
dim(OSE.sites)[1];    length(unique(OSE.sites$OSEWellID)) ; length(unique(OSE.sites$OBJECTID.OSE))
#There are so, so many duplicates of OSEWellID ... not a true unique number. 
#The OBJECTID.OSE is a unique number but can't match to GW levels
#find duplicates and keep last record
n_occur <- data.frame(table(OSE.sites$OSEWellID))
zt <- n_occur[n_occur$Freq > 1,]  
#62492 sites have one duplicate... these are not in the same county and have different lat/long
#remove those sites 
OSE.sites <- OSE.sites[! OSE.sites$OSEWellID %in% n_occur$Var1[n_occur$Freq > 1],]
summary(OSE.sites$AgencyCd)


#ABQ sites
dim(ABQ.sites)[1]; length(unique(ABQ.sites$SiteNo))  #no duplicates


##########################################################################################################################################################################################
sites.joined <- NULL
sites.joined <- rbind.fill(NGWMN.sites, USGS.sites)
#30 duplicates are between portal and USGS
n_occur <- data.frame(table(sites.joined$SiteNo))
zt <- n_occur[n_occur$Freq > 1,] 
sites.joined <- sites.joined[!rev(duplicated(rev(sites.joined$SiteNo))),]  

#continue join
sites.joined <- rbind.fill(sites.joined, NMBGMR.sites)
n_occur <- data.frame(table(sites.joined$SiteNo)) %>% filter(Freq>1)
sites.joined <- sites.joined[!rev(duplicated(rev(sites.joined$SiteNo))),] 
#11 duplicates

sites.joined <- rbind.fill(sites.joined, ABQ.sites)
n_occur <- data.frame(table(sites.joined$SiteNo)) %>% filter(Freq>1)
#no new duplicates

sites.joined <- rbind.fill(sites.joined, OSE.sites)
n_occur <- data.frame(table(sites.joined$SiteNo)) %>% filter(Freq>1)
#1 new duplicate

table(sites.joined$AgencyCd)

#We can add Aquifer Names using USGS reference list. We can also add County Fips, State Names, stuff like that
#Also need to add depth to water based on Nad 1988

sites.joined.skinny <- sites.joined %>%
  dplyr::select(AgencyCd, SiteNo, AgencyNm, SiteName, DecLatVa, DecLongVa, 
                HorzDatum, AltVa, AltDatumCd, CountyNm, SiteType, WellDepth, NatAquiferCd, 
                LocalAquiferCd, LocalAquiferName, 
                BasinCd, CasingSize, Comment)

#there is a duplicate by agency type so when we grab GWlevels with Agency Code and Site Number
n_occur <- data.frame(table(sites.joined.skinny$SiteNo)) %>% filter(Freq > 1)
zt <- sites.joined.skinny[sites.joined.skinny$SiteNo %in% n_occur$Var1,] %>% arrange(SiteNo)

#The first occurrence has more data than the second - so keep the first occurrence
sites.joined.skinny <- sites.joined.skinny[!(duplicated(sites.joined.skinny$SiteNo)),] 



saveRDS(sites.joined, "./Processed/sites.joined.rds")
saveRDS(sites.joined.skinny, "./Processed/sites.joined.skinny.rds")

#write.csv(sites.joined, file = "./Processed/sites.joined.csv")
#write.csv(sites.joined.skinny, file="./Processed/sites.joined.skinny.csv")


#--------create static gwl summarized df-----------#
gwl.joined.skinny <- readRDS("./Processed/gwl.joined.skinny.rds")
gwl.joined.skinny$Date <- as.Date(gwl.joined.skinny$Date)
gwl.joined.skinny.static <- gwl.joined.skinny %>%
  dplyr::group_by(SiteNo) %>%
  dplyr::summarise(
    firstMeas = min(Date),
    lastMeas = max(Date), 
    Count = length(SiteNo)
  )
#This df does not include any measurements from OSE because there is no unique site No.


length(unique(sites.joined.skinny$SiteNo))
length(unique(gwl.joined.skinny.static$SiteNo))

sites.summary.static <- base::merge(sites.joined.skinny, gwl.joined.skinny.static, 
                                    by.x = "SiteNo", by.y="SiteNo", all=TRUE)
length(unique(sites.summary.static$SiteNo))
n_occur <- data.frame(table(sites.summary.static$SiteNo)) %>% filter(Freq>1) #no duplicates

#Fix variables
sites.summary.static$firstMeas <- as.Date(sites.summary.static$firstMeas)
sites.summary.static$lastMeas <- as.Date(sites.summary.static$lastMeas)

#convert NA's in count to zero  
sites.summary.static$Count <- ifelse(is.na(sites.summary.static$Count)==TRUE, 0, 
                                     sites.summary.static$Count)

NAcounties <- sites.summary.static %>% filter(is.na(CountyNm))
#there are 894 NAs, either because of gwl site numbers with no related site data, or  ABQ didn't provide

#take out those without any site info
sites.summary.static <- sites.summary.static %>% filter(!is.na(CountyNm))
sites.summary.static$CountyNm <- as.character(sites.summary.static$CountyNm)

length(unique(gwl.joined.skinny.static$SiteNo))
countstatic <- sites.summary.static %>% filter(Count>1) 
length(unique(countstatic$SiteNo))#implies there are only 1642 sites with more than one gwl measurement?

saveRDS(sites.summary.static, file = "./Processed/sites.summary.static.rds")
str(sites.summary.static)

sites.summary.static10 <- head(sites.summary.static,10)
saveRDS(sites.summary.static10, file = "./Processed/sites.summary.static10.rds")
write.csv(sites.summary.static10, file = "./Processed/sites.summary.static10.csv")
