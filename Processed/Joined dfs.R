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
    "reliability_cd.USGS", "gw_file_cd.USGS", "NatAquiferCd", "LocalAquiferName", 
    "aqfr_type_cd.USGS", "WellDepth", "HoleDepth", "depth_src_cd.USGS",
    "project_no.USGS", "AgencyNm", "CountyNm")

names(NMBGMR.sites) <- c("DecLongVa", "DecLatVA", "SiteNo", "SiteID.NMBGMR", "Easting",
                         "Northing", "AltVa", "WellDepth", "FormationZone.NMBGMR",
                         "FormationMeaning.NMBGMR", "DepthToWater", "AgencyCd",
                         "AgencyNm", "CountyNm")

names(ABQ.sites) <- c("X", "facility_id.ABQ", "SiteNo", "SiteName", "data_provider",
                      "subfacility_code.ABQ", "loc_desc.ABQ","loc_type.ABQ","loc_purpose.ABQ",
                      "loc_type_2.ABQ","BasinCd","within_facility_yn.ABQ","CountyCd",
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
                      "station.ABQ","route.ABQ")

names(OSE.sites) <- c("DecLongVa", "DecLatVa", "AgencyCd", "AgencyNm","OBJECTID.OSE",
                      "pod_basin.OSE", "OSEWellID", "pod_suffix.OSE", "SiteName",
                      "AltVa", "TimeZone", "Easting", "Northing", "crs_code", "HorzDatum",
                      "StateCd", "CountyNm", "City", "Zip", "LocalAquiferName",
                      "WellDepth", "depth_water.OSE", "use_of_well.OSE", "CasingSize")

sites.joined <- rbind.fill(NGWMN.sites, USGS.sites)
sites.joined <- rbind.fill(sites.joined, NMBGMR.sites)
sites.joined <- rbind.fill(sites.joined, ABQ.sites)
sites.joined <- rbind.fill(sites.joined, OSE.sites)
names(sites.joined)

sites.joined.skinny <- sites.joined %>%
  select(AgencyCd, SiteNo, AgencyNm, SiteName, OSEWellID, DecLatVa, DecLongVa, 
         HorzDatum, AltVa, AltDatumCd, CountyNm, SiteType, WellDepth, LocalAquiferName, 
         BasinCd, CasingSize, Comment)

saveRDS(sites.joined, "./Processed/sites.joined.rds")
saveRDS(sites.joined.skinny, "./Processed/sites.joined.skinny.rds")

#write.csv(sites.joined, file = "./Processed/sites.joined.csv")
#write.csv(sites.joined.skinny, file="./Processed/sites.joined.skinny.csv")




#--------create static gwl summarized df-----------#
gwl.joined.skinny <- readRDS("./Processed/gwl.joined.skinny.rds")
gwl.joined.skinny$Date <- as.Date(gwl.joined.skinny$Date)
gwl.joined.skinny.static <- gwl.joined.skinny %>%
  dplyr::group_by(AgencyCd, SiteNo) %>%
  dplyr::summarise(
    firstMeas = min(Date),
    lastMeas = max(Date), 
    Count = length(SiteNo)
  )

sites.summary.static <- left_join(sites.joined.skinny, gwl.joined.skinny.static, by = "SiteNo")
saveRDS(sites.summary.static, file = "./Processed/sites.summary.static.rds")
