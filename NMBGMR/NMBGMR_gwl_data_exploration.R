library(sp); library(rgdal); library(maptools); library(gplots); library(rgeos); library(raster)
library(stringr); library(PBSmapping); library(spData); library(sf); library(plyr); library(maps)
library(ggplot2); 
#tidyverse messes up maps function because purrr masks part of it

options(scipen=999) #turns scientific notation to numeric

NMBGMR.gwl <- read.csv("./NMBGMR/gwLeveldata.csv") #134264 observations total

NMBGMR.gwl$DepthToWater <- ifelse(is.na(NMBGMR.gwl$MedDepth2WaterBGS), 
                                  NMBGMR.gwl$MedManualDepth2WaterBGS, 
                                  NMBGMR.gwl$MedDepth2WaterBGS)
summary(NMBGMR.gwl$DepthToWater)



NMBGMR.gwl$AgencyCd <- "NMBGMR"





#save file as csv
write.csv(NMBGMR.gwl, file="./NMBGMR/NMBGMR.gwl.csv")



