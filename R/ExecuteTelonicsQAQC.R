##################################################################################
## Project title: NWBF Lynx Project                                              #
## Author: Jared Laufenberg                                                      #
## Email: Jared_Laufenberg@fws.gov                                               #
## Date created: 06/15/2018                                                      #
## Date last edited: 12/16/2019                                                  #
##                                                                               #
## This script executes 2 custom R functions that process Telonics GPS collar    #
## data from collared Canada lynx and executes R code that compiles LaTeX and R  #
## code from an .Rnw file to a .tex file and then compiles that .tex file into a #
## PDF-formatted documentas part of the Northwest Boreal Forest Lynx Project.    #
## The batch.flat.gps.R script processes GPS location data only                  #
## retaining QFP (Quick-Fix Position) locations and aggregates those locations   #
## into a single flat-formatted data.frame.  The batch.flat.mort.R script        #
## processes location data and creates a flat-formatted data.frame containing    #
## information on mortality status for each lynx.                                #
##################################################################################

## Specify some control variables
archive <- TRUE # Value of FALSE writes report to working directory where code is located
createTeXdoc <- TRUE
writepdf <- TRUE
## WD needs to be specified for scheduling this as an automated task
if(archive){
    setwd("s:/InvMon/Biometrics/_projects/refuge/_regional_projects/lynx/code")
    report.destination <- "s:/im_archive/boreal_lynx/products/reports/telonics_qaqc"
}else{
    report.destination <- "l:/projects/ak_region/lynx/products/reports/telonics_qaqc"
}
## Process all QFP location data
source("./functions/batch.flat.gps.R")
batch.flat.gps(iridium_csv.dir="s:/im_archive/boreal_lynx/products/iridium_csv",
               save.file=TRUE,
               save.dir="../data/derived_data/gps_collar/telonics",
               returnx=FALSE,
               pattern="Complete")
## Process all mortality notification data
source("./functions/batch.flat.mort.R")
batch.flat.mort(iridium_csv.dir="s:/im_archive/boreal_lynx/products/iridium_csv",
                save.file=TRUE,
                save.dir="../data/derived_data/gps_collar/telonics",
                returnx=FALSE,
                pattern="Complete")
# Load packages
library(knitr)
##
## Compile .Rnw file containing LaTeX and R code into .tex file
if(createTeXdoc){
    report.name <- paste0("TelonicsQAQCreport_",gsub("-","",Sys.Date(),fixed=TRUE),".tex")
    knitr::knit(input="TelonicsQAQCknitr.Rnw",
                output=paste0("../incoming/temp/",report.name))
}
## Change wd to location where report will be written
setwd("../incoming/temp")
## Compile .tex files into PDF document
if(writepdf){tools::texi2pdf(file=report.name)}
## Copy report to specified destination folder
if(writepdf){
    report.name <- paste0("TelonicsQAQCreport_",gsub("-","",Sys.Date(),fixed=TRUE),".pdf")
    file.copy(from=report.name,
              to=paste0(report.destination, "/", report.name))
}
## Remove auxiliary files and 'figure' directory
file.remove(list.files()[list.files()!="figure"])
unlink("figure",recursive=TRUE)
