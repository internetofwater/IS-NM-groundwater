##NMBGMR loop to get water level data for each site
library(tidyverse)
library(httr) #for http error
library(lubridate)
library(varhandle) #to unfactor factors to turn into numeric

NMBGMR.site <- read.csv("./NMBGMR/NMBGMR_SiteInfo.csv")


#double check that PointID is the thing I want for the unique number.
siteNo.list <- unique(NMBGMR.site$PointID)
#siteNo <- "BC-0030"

baseURL <- 'https://maps.nmt.edu/maps/data/export_hydrograph/'
fileType <- '.csv'
i=1
projectsURL = paste0(baseURL,siteNo.list[i],fileType)

#create gw.meta table from first 7 rows
gw.meta <- read.csv(projectsURL, nrows=7, header=FALSE, as.is=TRUE) %>% as.data.frame()
headers <- c("PointID", gw.meta$V1)
values <- c("skeleton", gw.meta$V2)

#create data frame
gw.meta <- as.data.frame(matrix(nrow=0,ncol=8))
colnames(gw.meta) <- headers
gw.meta[1,] <- values
gw.meta$PointID <- as.factor(gw.meta$PointID)

#create gw.levels table from after the first 7 rows
gw.lev <- read.csv(projectsURL, 
                   skip=7, header=FALSE, na.strings="", as.is=TRUE) %>% as.data.frame()
gw.lev <- gw.lev[-1,]
gw.lev$PointID <- as.factor("skeleton")

colnames(gw.lev) <- c("DateMeasured", "Depth2WaterBGS", "ManualDepth2WaterBGS",
                      "Status", "MeasurementMethod", "DataSource", "MeasuringAgency",
                      "Notes", "PointID")

gw.lev <- gw.lev[!with(gw.lev,is.na(Depth2WaterBGS) & is.na(ManualDepth2WaterBGS)),]

gw.lev$Date <- (substr(gw.lev$DateMeasured, start=1, stop=10))
gw.lev$Date <- as_date(gw.lev$Date)


#consolidate measurements by date
gw.avg <- gw.lev %>% dplyr::group_by(PointID, Date, MeasurementMethod, DataSource, 
                             MeasuringAgency) %>%
                     summarise(DTW = median(Depth2WaterBGS, na.rm =TRUE),
                               Manual.DTW = median(`ManualDepth2WaterBGS`, na.rm=TRUE)) 



for (i in siteNo.list[1]) {
  #siteNo <- "BC-0030"
  projectsURL <- paste0(baseURL,siteNo.list[i],fileType)
  if(http_error(projectsURL) == TRUE) {
    meta <- c(siteNo.list[i], "NA", "NA", "NA", "NA", "NA", "NA", "NA")
    lev <- c(siteNo.list[i], "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
    
  } else if(http_error(projectsURL) == FALSE) {
    meta <- read.csv(projectsURL, nrows=7, header=FALSE, as.is=TRUE) %>% as.data.frame()
    meta <- c(siteNo.list[i], meta$V2)   
    
    lev <- read.csv(projectsURL, 
                       skip=7, header=FALSE, na.strings="", as.is=TRUE) %>% as.data.frame()
    lev <- lev[-1,]
    lev$PointID <- siteNo.list[i]
    colnames(lev) <- c("DateMeasured", "Depth2WaterBGS", "ManualDepth2WaterBGS",
                          "Status", "MeasurementMethod", "DataSource", "MeasuringAgency",
                          "Notes", "PointID")
    
    lev <- lev[!with(lev,is.na(Depth2WaterBGS) & is.na(ManualDepth2WaterBGS)),]
    
    lev$Date <- (substr(lev$DateMeasured, start=1, stop=10))
    lev$Date <- as_date(lev$Date)
    
    
    
    #consolidate measurements by date
    avg <- lev %>% dplyr::group_by(PointID, Date, MeasurementMethod, DataSource, 
                                         MeasuringAgency) %>%
      summarise(DTW = median(Depth2WaterBGS, na.rm =TRUE),
                Manual.DTW = median(`ManualDepth2WaterBGS`, na.rm=TRUE))
    
  }
  gw.meta <- rbind(gw.meta, meta)
  gw.avg <- rbind(gw.avg, avg)
  print(siteNo.list[i])
}
  
  
  
for (i in siteNo.list[1:10]) {
  print(i)
}
  

