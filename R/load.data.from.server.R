################################################################################
# Load the Lynx Collar Data from the I&M Server and Save it Locally
#
# Author: McCrea Cobb <mccrea_cobb@fws.gov>
# Date created: 9/7/2018
# Date edited: 2/15/2019

# Changed the out to a RData file in an attempt to remove all data clean up
# functions from the Shiny app.
################################################################################


#-------------------------------------------------------------------------------
## Update the flat.file on the server (make sure that the server drive is mapped first):

setwd("s:/InvMon/Biometrics/_Projects/Refuge/_RegionalProjects/Lynx/Scripts")
## Process all QFP location data
source("./Functions/batch.flat.gps.R")
batch.flat.gps(iridium_csv.dir="../Data/RawData/GPSCollar/telonics/iridium_csv",
               save.file=TRUE,
               save.dir="../Data/DerivedData/GPSCollar/telonics",
               returnx=FALSE,
               pattern="Complete")
## Process all mortality notification data
source("./Functions/batch.flat.mort.R")
batch.flat.mort(iridium_csv.dir="../Data/RawData/GPSCollar/telonics/iridium_csv",
                save.file=TRUE,
                save.dir="../Data/DerivedData/GPSCollar/telonics",
                returnx=FALSE,
                pattern="Complete")

#-------------------------------------------------------------------------------
## The Shiny app does not allow us to publish to shinyapps.io with a remote
## file path in the script. So, my workaround is to pull out the function for
## updating the data from the server and saving it locally. Then, the shiny script
## works from the local copy



GetServerData <- function(){
  require(tidyverse)
  require(dplyr)
  # Loads the updated flat.file from the I&M server and saves it locally:
  
  # Set working directory:
  setwd("f:/ActiveProjects/Shiny/Lynx")
  
  # Load the data:
  dat <- "s:/InvMon/Biometrics/_Projects/Refuge/_RegionalProjects/Lynx/Data/DerivedData/GPSCollar/telonics/flat.gps.gzip"
  dat <- get(load(dat)) ; rm(x)
  dat.collar <- read.csv("s:/InvMon/Biometrics/_Projects/Refuge/_RegionalProjects/Lynx/Data/RawData/Capture/CaptureData.csv")
  dat.collar$id <- dat.collar$CTN
  
  dat$date <- as.Date(dat$fixtime)
  dat$month <- as.numeric(format(dat$date, "%m"))
  dat <- dat[complete.cases(dat$lat), ]   # Remove NA rows from lat/lon
  
  # Make datecapture a POSIXct:
  dat.collar$datecapture <- as.POSIXct(strptime(as.character(dat.collar$DateCapture), format="%m/%d/%Y"))
  
  dat <- merge(dat, dat.collar, by="id") ; rm(dat.collar)
  
  # Convert all factors to characters:
  i <- sapply(dat, is.factor)
  dat[i] <- lapply(dat[i], as.character) ; rm(i)
  
  # Subset out columns not needed for the app to reduce file size:
  dat <- dat %>% dplyr::select(-c(utmzone, utmy, utmx, alt, fixsched, fixtype, hdop, 
                                  nsats, step, angle, deploy.site, Primary, CTN,
                                  Aux1, Aux2, Aux3, Code, ID, DateCapture))
  
  # Remove duplicate fixes:
  dat <- dat %>% distinct(id, fixtime, .keep_all = T)
  
  # 700521A (KNI): remove fixes after it was trapped:
  dat <- dat %>% filter(!(id == "700521A" & date > as.Date("2018-10-28")))
  
  # 700552A (YF): remove fixes after it died:
  dat <- dat %>% filter(!(id == "700552A" & date > as.Date("2018-06-18")))
  
  # 700534A (Kanuti): remove fixes after it died (? or something):
  dat <- dat %>% filter(!(id == "700534A" & date > as.Date("2018-05-04")))
  
  # Save it:
  save(dat, file="./data/collardata.RData")
}


GetServerData()
