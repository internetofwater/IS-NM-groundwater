pacman::p_load(tidyverse, plyr, dataRetrieval, devtools)



#--------joining gwl dfs-----------#

#NGWMN = NGWMN.gwl.skinny and USGS = USGS.gwl

NGWMN.gwls <- read.csv("./NGWMN/NGWMN.gwl.skinny.csv")
NGWMN.gwls <- NGWMN.gwls %>% select(-X)
USGS.gwls <- read.csv("./USGS/USGS.gwl.csv")
USGS.gwls <- USGS.gwls %>% select(-X)

names(USGS.gwls)
names(NGWMN.gwls)
#rename so that all overlapping variables with NGWMN have the same name in NGWMN
#sl_lev_va matched with "Water.level.in.feet.relative.to.NAVD.88" 
  #BUT we don't know the vertical datum (sl_datum_cd)
names(USGS.gwls) <- c("AgencyCd", "SiteNo", "site_tp_cd.USGS", "Date", "Time", 
                     "lev_tz_cd_reported.USGS", "Depth.to.Water.Below.Land.Surface.in.ft.",
                     "Water.level.in.feet.relative.to.NAVD88", "sl_datum_cd.USGS", 
                     "lev_status_cd.USGS", "Data.Provided.by", "lev_dt_acy_cd.USGS",
                     "Accuracy.Value", "lev_src_cd.USGS", "Observation.Method", 
                     "Comment", "DateTime", "lev_tz_cd.USGS")
#all columns that relate to NGWMN have been renamed using the NGWMN column names.
#The columns that don't relate to NGWMN have been kept as is plus .USGS

gwl.joined <- rbind.fill(NGWMN.gwls, USGS.gwls)
gwl.joined.skinny <- gwl.joined %>%
  select(AgencyCd, SiteNo, Date, Time, Depth.to.Water.Below.Land.Surface.in.ft., Accuracy.Value,
         Water.level.in.feet.relative.to.NAVD88, Observation.Method, Comment)

write.csv(gwl.joined.skinny, file="./Processed/gwl.joined.skinny.csv")


#--------joining site info dfs-----------#

#USGS = sites.combined, NGWMN = NGWMN.site.skinny, NMBGMR = NMBGMR.site
USGS.sites <- read.csv("./USGS/USGS.site.csv")
USGS.sites <- USGS.sites %>% select(-X)
NGWMN.sites <- read.csv("./NGWMN/NGWMN.site.skinny.csv") 
NGWMN.sites <- NGWMN.sites %>% select(-X)
NMBGMR.sites <- read.csv("./NMBGMR/NMBGMR.site.csv")
NMBGMR.sites <- NMBGMR.sites %>% select(-X)


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

sites.joined <- rbind.fill(sites.joined, NMBGMR.sites)
names(sites.joined)

sites.joined.skinny <- sites.joined %>%
  select(AgencyCd, SiteNo, AgencyNm, SiteName, OSEWellID, DecLatVa, DecLongVa, 
         HorzDatum, AltVa, AltDatumCd, CountyNm, SiteType, WellDepth)

write.csv(sites.joined.skinny, file="./Processed/sites.joined.skinny.csv")

gwl.sites.joined <- left_join(gwl.joined.skinny, sites.joined.skinny, 
                              by = c("SiteNo", "AgencyCd"))

write.csv(gwl.sites.joined, file = "./Processed/gwl.sites.joined.csv")


