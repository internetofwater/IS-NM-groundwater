##NMBGMR loop to get water level data for each site

NMBGMR.site <- read.csv("./NMBGMR/NMBGMR_SiteInfo.csv")

siteNo.list <- NMBGMR.site$PointID

baseURL <- 'https://maps.nmt.edu/maps/data/export_hydrograph/'
fileType <- '.csv'

i=1
projectsURL = paste0(baseURL,siteNo.list[i],fileType)
dat <- read.csv(url(projectsURL))

for (i in 2:length(siteNo.list)){
  projectsURL = paste0(baseURL,siteNo.list[i],fileType)
  foo <- read.csv(url(projectsURL))
  foo <- foo %>% mutate_all(as.character)
  
  dat <- rbind(dat, foo)
  print(siteNo.list[i])
}
