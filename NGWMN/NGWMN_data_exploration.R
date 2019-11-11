#NGWMN Data

pacman::p_load(tidyverse, sf, maps, sp)

getwd()
#water level info
NGWMN.gwl <- read.csv("./NGWMN/Raw/WATERLEVEL.csv"); head(NGWMN.gwl)
#taking out unused/irrelevant columns
NGWMN.gwl.skinny <- NGWMN.gwl %>%
  select(AgencyCd, SiteNo, Time, Depth.to.Water.Below.Land.Surface.in.ft., 
         Water.level.in.feet.relative.to.NAVD88, Observation.Method, Accuracy.Value,
         Data.Provided.by, Comment)

#exploratory
summary(NGWMN.gwl$Original.Direction) #not sure what this is
summary(NGWMN.gwl$Original.Parameter) #parameter must be gwl?
summary(NGWMN.gwl$Original.Value) 
summary(NGWMN.gwl$Depth.to.Water.Below.Land.Surface.in.ft.) 
#Depth.to.Water...is the same as Original.Value


#fixing time components
NGWMN.gwl.skinny <- NGWMN.gwl.skinny %>% rename_at("Time",~"DateTime")
NGWMN.gwl.skinny$Date <- as.Date(NGWMN.gwl.skinny$DateTime) 
#fix time component so that all time values are not 00:00 - see Lauren's email
NGWMN.gwl.skinny$DateTime <- as.POSIXct(NGWMN.gwl.skinny$DateTime)
NGWMN.gwl.skinny$Time <- format(as.POSIXct(NGWMN.gwl.skinny$DateTime), format = "%H:%M:%S")

write.csv(NGWMN.gwl.skinny, file="./NGWMN/NGWMN.gwl.skinny.csv") 

#site info
NGWMN.site <- read.csv("./NGWMN/Raw/SITE_INFO.csv"); head(NGWMN.site)
#taking out unused/irrelevant columns
NGWMN.site.skinny <- NGWMN.site %>%
  select(AgencyCd, SiteNo, AgencyNm, SiteName, DecLatVa, DecLongVa, HorzDatum, HorzMethod,
         HorzAcy, AltVa, AltDatumCd, AltMethod, AltAcy, StateCd, StateNm, CountyCd, CountyNm,
         SiteType, WellDepth, NatAquiferCd, NatAqfrDesc, LocalAquiferCd, LocalAquiferName)
unique(NGWMN.site.skinny$SiteNo) #58 site names
write.csv(NGWMN.site.skinny, file="./NGWMN/NGWMN.site.skinny.csv")


#---------to plot--------------#

#combine gwl info with site info
NGWMN.combined <- 
  left_join(NGWMN.gwl.skinny, NGWMN.site.skinny, by=c("AgencyCd","SiteNo"))
class(NGWMN.combined$Date)
summary(NGWMN.combined$Depth.to.Water.Below.Land.Surface.in.ft.)

#plot depth to see range
ggplot(NGWMN.combined, (aes(x=SiteNo, y=Depth.to.Water.Below.Land.Surface.in.ft.))) +
  geom_point() +
  labs(x= "Site No.", y = "Water level below ground surface (ft)")

#create spatial dataset of combined df
NGWMN.combined.spatial <- st_as_sf(NGWMN.combined,
                                      coords=c("DecLongVa", "DecLatVa"), crs = 4269)
unique(NGWMN.combined.spatial$geometry) #only 56 unique geometries - 2 sites with same location?
proj <- st_crs(NGWMN.combined.spatial) #lat and long are in 4269

states <- st_as_sf(map(database = "state", plot = TRUE, fill = TRUE, col = "white"))
NM.map <- states %>% filter(ID == "new mexico")
NM.map<- st_set_crs(NM.map, proj) #transform to match spatial datafram
st_crs(NM.map) 


#plot
NGWMNsites.map <- ggplot() +
  geom_sf(data = NM.map, fill = "white") +
  geom_sf(data = gwlsites.combined.spatial, 
          aes(color = Depth.to.Water.Below.Land.Surface.in.ft.), 
          alpha = 0.5, size = 1) +
  scale_color_viridis_c(direction =-1) +
  labs(color = "Water level below ground surface") +
  theme(legend.position = "top")
print(NGWMNsites.map)


-----------##Joining datasets##----------------

NGWMN.USGS.combined <- 
  inner_join(NGWMN.combined, USGS.combined, 
             by = c("AgencyCd", "SiteNo", "SiteType", "Date", "Time", 
                    "Depth.to.Water.Below.Land.Surface.in.ft.", 
                    "Water.level.in.feet.relative.to.NAVD88",
                    "Data.Provided.by", "Accuracy.Value", "Observation.Method", 
                    "Comment", "DateTime", "SiteName", "DecLatVa", "DecLongVa",
                    "HorzMethod", "HorzAcy", "HorzDatum", "CountyCd", "AltVa",
                    "AltMethod", "AltAcy", "AltDatumCd", "NatAquiferCd", 
                    "LocalAquiferCd", "WellDepth"))
  
#how do I organize, but still make it easy for others to copy and replicate this code?
