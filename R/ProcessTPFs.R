####################################################################################
## Project title: NWBF Lynx Project                                                #
## Author: Jared Laufenberg                                                        #
## Email: Jared_Laufenberg@fws.gov                                                 #
## Date created: 02/26/2019                                                        #
## Date last edited: 06/11/2019                                                    #
##                                                                                 #
## This script extracts CTN and IMEI values for each Iridium GPS collar from       #
## Telonics Program Files (TPF) stored in PDF format. Those values are combined    #
## with collar-specific fix-rate program settings also derived from PDF versions   #
## of TPF files.  The final product is a TelonicsCollarPrograms table stored as    #
## CSV format in the "../data/raw_data/gps_collar/telonics" directory. This script #
## depends on several custom functions stored in the "./functions" subfolder and   #
## also depends on a single reference table (CTN_Site_ref.csv) that lists the      #
## deployment site (study area) for each deployed collar. The reference table      #
## will eventually be replaced by querying the capture data base.                  #
##                                                                                 #
## Note: Only run this script when new TFP files are added or when the Site        #
## field in the reference table (or capture data base in the future) is updated.   #
####################################################################################

## Source in custom functions
source("./functions/get.CTNs.R")
source("./functions/get.IMEIs.R")
source("./functions/get.fixscheds.R")
source("./functions/addSites.R")
## create local custom function
create.df <- function(ctn,fixscheds,tpfname){
    n <- length(ctn)
    df <- data.frame(Primary=rep(fixscheds[1],n),
                     Aux1=rep(fixscheds[2],n),
                     Aux2=rep(fixscheds[3],n),
                     Aux3=rep(fixscheds[4],n),
                     TPF=rep(tpfname,n))
    df
}
## Process files
files <- list.files("../data/raw_data/gps_collar/telonics/telonics_tpf", pattern=".pdf", full=TRUE)
files <- files[-grep("email",files)]
CTNSiteRef <- read.csv("../data/raw_data/gps_collar/telonics/CTN_Site_ref.csv")
fs = lengths(regmatches(files, gregexpr("/", files)))[1]
tpfnames <- gsub(".pdf",".tpf",read.table(text=files, sep="/", as.is=TRUE)[,7])
ctn <- lapply(files,get.CTNs)
fix.scheds <- lapply(files,get.fixscheds)
df <- do.call("rbind",mapply(create.df,ctn=ctn,fixscheds=fix.scheds,tpfname=tpfnames,SIMPLIFY=FALSE))
imei <- paste0("IMEI",unlist(lapply(files,get.IMEIs)))
nas <- rep(NA,nrow(df))
collarIDs <- data.frame(CTN=unlist(ctn), IMEI=imei,df,
                        Site=nas, DateProgrammed=nas, ProgrammedBy=nas,
                        DateActivated=nas, DateDeactivated=nas, LiNCTerminated=nas)
collarIDs <- collarIDs[order(as.numeric(substr(collarIDs$CTN,1,6))),]
collarIDs <- addSites(collarIDs,CTNSiteRef)
## Save output
write.csv(collarIDs,file="../data/raw_data/gps_collar/telonics/TelonicsCollarPrograms.csv",row.names=FALSE)


