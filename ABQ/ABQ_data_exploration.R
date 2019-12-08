library(tidyverse)
library(sf)
library(readxl)
library(maps)
library(sp)
library(rgdal)

# read in site data
path <- "C:/Users/19524/Documents/DUKE/Independent Study - LP/IS-NM-groundwater/ABQ/"
fileName <- "CityABQ_DT_Location_Example_Dictionary.xlsx"

ABQ.site <- read_excel(paste0(path, fileName), sheet="ExampleEntry")

# read in gwl data

filename2 <- "CityABQ_DT_Water_Level_Data.xlsx"

ABQ.gwl <- read_excel(paste0(path, filename2), sheet = "Sheet2")

