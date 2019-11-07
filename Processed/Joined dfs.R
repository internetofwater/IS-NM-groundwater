pacman::p_load(tidyverse, plyr, dataRetrieval)


#--------joining gwl dfs-----------#

#NGWMN = NGWMN.gwl.skinny and USGS = USGS.gwl

names(USGS.gwl)
names(NGWMN.gwl.skinny)
#rename so that all overlapping variables with NGWMN have the same name in NGWMN
#sl_lev_va matched with "Water.level.in.feet.relative.to.NAVD.88" 
  #BUT we don't know the vertical datum (sl_datum_cd)
names(USGS.gwl) <- c("AgencyCd", "SiteNo", "site_tp_cd.USGS", "Date", "Time", 
                     "lev_tz_cd_reported.USGS", "Depth.to.Water.Below.Land.Surface.in.ft.",
                     "Water.level.in.feet.relative.to.NAVD88", "sl_datum_cd.USGS", 
                     "lev_status_cd.USGS", "Data.Provided.by", "lev_dt_acy_cd.USGS",
                     "Accuracy.Value", "lev_src_cd.USGS", "Observation.Method", 
                     "Comment", "DateTime", "lev_tz_cd.USGS")
#all columns that relate to NGWMN have been renamed using the NGWMN column names.
#The columns that don't relate to NGWMN have been kept as is + .USGS

gwl.joined <- rbind.fill(NGWMN.gwl.skinny, USGS.gwl)
gwl.joined.skinny <- gwl.joined %>%
  select(AgencyCd, SiteNo, Date, Time, Depth.to.Water.Below.Land.Surface.in.ft., Accuracy.Value,
         Water.level.in.feet.relative.to.NAVD88, Observation.Method, Comment)


#--------joining site info dfs-----------#

#USGS = sites.combined, NGWMN = NGWMN.site.skinny

str(USGS.site1)
str(NGWMN.site.skinny)                         

names(NGWMN.site.skinny)
names(USGS.site)

names(USGS.site) <- 
  c("AgencyCd", "SiteNo", "SiteName", "SiteType", "DecLatVa", "DecLongVa", 
    "lat_va.USGS", "long_va.USGS", "HorzMethod", "HorzAcy", "coord_datum_cd.USGS",
    "HorzDatum", "district_cd.USGS", "StateCd", "CountyCd", "country_cd.USGS", 
    "land_net_ds.USGS", "map_nm.USGS", "map_scale_fc.USGS", "AltVa", "AltMethod", 
    "AltAcy", "AltDatumCd", "huc_cd.USGS", "basic_cd.USGS", "topo_cd.USGS", 
    "construction_dt.USGS", "inventory_dt.USGS", "drain_area_va.USGS", 
    "contrib_drain_area_va.USGS", "tz_cd.USGS", "local_time_fg.USGS", 
    "reliability_cd.USGS", "gw_file_cd.USGS", "NatAquiferCd", "LocalAquiferCd", 
    "aqfr_type_cd.USGS", "WellDepth", "HoleDepth", "depth_src_cd.USGS",
    "project_no.USGS", "AgencyNm")

sites.joined <- rbind.fill(NGWMN.site.skinny, USGS.site)



