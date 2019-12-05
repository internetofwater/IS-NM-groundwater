#Read in NM data from NMGBRM

#load library
library(jsonlite); library(tidyverse); library(lubridate); library(chron)
library(rvest); library(dplyr); library(purrr); library(stringr); library(httr)


rm(list=ls()) #removes anything stored in memory

#Read in NM data
baseURL = 'https://maps.nmt.edu/maps/data/export_hydrograph/'

#read in site data
siteSWD = 'C:\\Users\\lap19\\Documents\\Internet of Water\\Pilots\\NM Groundwater\\Crosswalk\\NMGeologyData\\'
siteName <- 'NMBGMR_SiteInfo'

siteData <- read.csv(paste0(siteSWD,siteName,'.csv'),sep=',', header=TRUE)
  head(siteData)

#create unique list of point ID to scrape web
uniquePoint <- unique(as.character(siteData$PointID))

#test
foo <- read.csv(paste0(baseURL,"SO-0246",".csv"))
head(foo)


#### Create site metadata frame -------------------------------------------------------------------------------------------
siteID <- "SO-0246"
siteMetadata <- read.csv(paste0(baseURL,siteID,".csv"), header = F, nrows = 7, as.is = T) %>% as.data.frame()
  header <- c("pointID", siteMetadata$V1)
  values <- c(siteID, siteMetadata$V2)

#create dataframe
gw.metadata <- as.data.frame(matrix(nrow=0,ncol=8))
  colnames(gw.metadata) <- header
gw.metadata[1,] <- values
#################################################################################################################################

#### Create groundwater level frame -------------------------------------------------------------------------------------------
gwData <- read.csv(paste0(baseURL,siteID,".csv"), header = F, skip=8, as.is = T, sep=",") %>% as.data.frame()
#set colnames
colnames(gwData) <- c("MeasureDate","Depth2WaterBGS","ManualDepth2WaterBGS","Status","Method","DataSource","MeasuringAgency","Notes")
head(gwData) #if make first one a header it messes up the columns for some reason
#add column with pointID
gwData$pointID <- siteID

#remove rows with no gw level data (sometimes ntoes)
gwData <- gwData[!with(gwData,is.na(Depth2WaterBGS) & is.na(ManualDepth2WaterBGS)),]

#Fix dates
gwData$Date <- as.Date(substr(as.character(gwData$MeasureDate),1,10),format="%Y-%m-%d")
#gwData$Time <- as.chron(substr(as.character(gwData$MeasureDate),12,20), format="%H:%M:%S")


#Loop through and find median by date
gwAveData <- gwData %>% group_by(pointID,Date,Status,Method) %>% 
  summarize(MedDepth2WaterBGS = median(Depth2WaterBGS, na.rm=TRUE), 
            MedManualDepth2WaterBGS = median(ManualDepth2WaterBGS, na.rm=TRUE)) %>% as.data.frame();
plot(gwAveData$Date, gwAveData$MedDepth2WaterBGS, type='l')
bkup <- gwAveData
#################################################################################################################################



#### Loop through Sites to create data frame #################################################################################
#since we set up the data frame with "SO-0246", remove from unique ID and loop through
uniquePoint <- uniquePoint[uniquePoint != siteID]

#loop through
for (i in 5417:length(uniquePoint)){
  siteID <- uniquePoint[i];
  
  #check for http error
  if(http_error(paste0(baseURL,siteID,".csv"))==FALSE){
  
    #read in metadata
     foo.meta <- read.csv(paste0(baseURL,siteID,".csv"), header = F, nrows = 7, as.is = T) %>% as.data.frame()
     values <- c(siteID, foo.meta$V2)
    #add to existing dataframe
     gw.metadata[(i+1),] <- values
     
     #read in gwlevels
     foo.gw <- read.csv(paste0(baseURL,siteID,".csv"), header = F, skip=8, as.is = T, sep=",") %>% as.data.frame()
      colnames(foo.gw) <- c("MeasureDate","Depth2WaterBGS","ManualDepth2WaterBGS","Status","Method","DataSource","MeasuringAgency","Notes")
      foo.gw$pointID <- siteID
      
      #check that last row is a date and not a note. If a note - delete. Delete any rows that have an NA for both GW depth columns
      foo.gw <- foo.gw[!with(foo.gw,is.na(Depth2WaterBGS)& is.na(ManualDepth2WaterBGS)),]
      #Fix dates
      foo.gw$Date <- as.Date(substr(as.character(foo.gw$MeasureDate),1,10),format="%Y-%m-%d")
  
      #Loop through and find median by date
      foo.gwave <- foo.gw %>% group_by(pointID,Date,Status,Method) %>% 
        summarize(MedDepth2WaterBGS = median(Depth2WaterBGS, na.rm=TRUE), 
                  MedManualDepth2WaterBGS = median(ManualDepth2WaterBGS, na.rm=TRUE)) %>% as.data.frame();
      
      #add to existing dataframe
      gwAveData <- rbind(gwAveData, foo.gwave)
  }
  
  #check for http error
  if(http_error(paste0(baseURL,siteID,".csv"))==TRUE){
    gw.metadata[(i+1),] <- c(siteID,NA,NA,NA,NA,NA,NA,NA)
    foo.gwave <- c(siteID,NA,NA,NA,NA,NA)
    gwAveData <- rbind(gwAveData, foo.gwave)
  }
  
  print(i)
}  
length(unique(gw.metadata$pointID))
length(unique(gwAveData$pointID))

gw.metadata$well_depth <- as.numeric(as.character(gw.metadata$well_depth))
gw.metadata$altitude <- round(as.numeric(as.character(gw.metadata$altitude)),4)
gw.metadata$formation_meaning <- str_remove_all(gw.metadata$formation_meaning, "[('),]") #remove extra values in string
  gw.metadata$formation_meaning <- ifelse(gw.metadata$formation_meaning=="---" | gw.metadata$formation_meaning=="",NA, gw.metadata$formation_meaning)

summary(gw.metadata) 

gwAveData$MedDepth2WaterBGS <- as.numeric(as.character(gwAveData$MedDepth2WaterBGS))
gwAveData$MedManualDepth2WaterBGS <- as.numeric(as.character(gwAveData$MedManualDepth2WaterBGS))
summary(gwAveData)

#write csv to file
write.csv(gw.metadata, 'C:\\Users\\lap19\\Documents\\Internet of Water\\Pilots\\NM Groundwater\\Crosswalk\\NMGeologyData\\gwLevelMetadata.csv', row.names=FALSE)
write.csv(gwAveData, 'C:\\Users\\lap19\\Documents\\Internet of Water\\Pilots\\NM Groundwater\\Crosswalk\\NMGeologyData\\gwLevelData.csv', row.names=FALSE)

### IT TOOK ABOUT 5 HOURS TO CODE UP AND RUN. WEBSITE PERIODICALLY WENT DOWN
bkup <- gw.metadata
################################################################################################################################################################################################



################################################################################################################################################################################################
#   ADD TO METADATA THE NUMBER OF GW LEVEL POINTS
################################################################################################################################################################################################

n.gwlevels <- gwAveData %>% group_by(pointID) %>% summarise(countDepth2Water = sum(!is.na(MedDepth2WaterBGS)), countManualD2W = sum(!is.na(MedManualDepth2WaterBGS))) %>% as.data.frame();
n.gwlevels$totalCount <- n.gwlevels$countDepth2Water+n.gwlevels$countManualD2W
summary(n.gwlevels)

gw.metadata <- merge(gw.metadata, n.gwlevels, by.x="pointID", by.y="pointID", all.x=TRUE)
gw.metadata <- gw.metadata[!with(gw.metadata,is.na(pointID)),]

zt <- subset(gwAveData, pointID=="SM-0049")
  plot(zt$Date, zt$MedDepth2WaterBGS, type="l", ylim=c(300,0), ylab="Depth to Water (Ft)", xlab="")
     points(zt$Date, zt$MedManualDepth2WaterBGS, col="blue", pch=19, cex=0.8)
dim(subset(n.gwlevels, totalCount<=1))


