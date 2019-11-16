##NMBGMR loop to get water level data for each site
library(tidyverse)
library(httr)
library(lubridate)

NMBGMR.site <- read.csv("./NMBGMR/NMBGMR_SiteInfo.csv")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


#double check that PointID is the thing I want for the unique number.
siteNo.list <- NMBGMR.site$PointID

baseURL <- 'https://maps.nmt.edu/maps/data/export_hydrograph/'
fileType <- '.csv'

i=1
projectsURL = paste0(baseURL,siteNo.list[i],fileType)

#create gw.meta table from first 7 rows
gw.meta <- read.csv(projectsURL, nrows=7, header=FALSE, as.is=TRUE) %>% as.data.frame()
headers <- c("PointID", gw.meta$V1)
values <- c(siteNo.list[i], gw.meta$V2)

#create data frame
gw.meta <- as.data.frame(matrix(nrow=0,ncol=8))
colnames(gw.meta) <- headers
gw.meta[1,] <- values

#create gw.levels table from after the first 7 hows
gw.lev <- read.csv(projectsURL, 
                   skip=7, header=FALSE) %>% as.data.frame()
colnames(gw.lev) <- as.character(unlist(gw.lev[1,])) 
gw.lev <- gw.lev[-1,]
gw.lev$PointID <- siteNo.list[i]
colnames(gw.lev) <- c("DateMeasured", "Depth2WaterBGS", "ManualDepth2WaterBGS",
                      "Status", "MeasurementMethod", "DataSource", "MeasuringAgency",
                      "Notes", "PointID")
gw.lev$Date <- (substr(gw.lev$DateMeasured, start=1, stop=10))
gw.lev$Date <- as_date(gw.lev$Date)
gw.lev$DepthToWaterBGS <- as.numeric.factor(gw.lev$DepthToWaterBGS)

#FIX THIS!
gw.lev$`Manual DepthToWaterBGS` <- as.numeric.factor(gw.lev$`Manual DepthToWaterBGS`)
gw.lev$`Manual DepthToWaterBGS` <- as.numeric(gw.lev$`Manual DepthToWaterBGS`)
gw.lev<- gw.lev %>% as.data.frame()
gw.lev <- gw.lev[!with(gw.lev,is.na(Depth2WaterBGS) & is.na(ManualDepth2WaterBGS)),]

#consolidate measurements by date
gw.avg <- gw.lev %>% dplyr::group_by(PointID, Date, MeasurementMethod, DataSource, 
                             MeasuringAgency) %>%
                     summarise(DTW = median(Depth2WaterBGS, na.rm =TRUE),
                               Manual.DTW = median(`ManualDepth2WaterBGS`, na.rm=TRUE))

plot(gw.avg$Date, gw.avg$DTW, type='l')



for (siteNo.list[i]) {
  if(http-error(projectsURL) = TRUE) {
    gw.meta[1,] <- c(siteNo.list[i], "NA", "NA", "NA", "NA", "NA", "NA", "NA")
    gw.avg
    
  }
  
  if(http-error(projectsURL) = FALSE) {
    
  }
  
}
  
  
  
  
  
  
  
for (file) { 
  if(http error = TRUE){
    values(PointID, Na, NA, ...)
  }}


for (pointID)
  if(http)
    )
  

dat <- read.csv(url(projectsURL))
  
for (i in 2:length(siteNo.list)){
  projectsURL = paste0(baseURL,"BC-0030",fileType)
  foo <- read.csv(url(projectsURL))
  foo <- foo %>% mutate_all(as.character)
  
  dat <- rbind(dat, foo)
  print(siteNo.list[i])
}

  
  
  
  
 

