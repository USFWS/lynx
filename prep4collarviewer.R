

## attach packages
library(AniTrackTools)
library(lynx)
## load and compile telonics csv file directly from server
gps <- import_tel_gps(path = list.files("s:/im_archive/boreal_lynx/products/iridium_csv",
                                           full.names = TRUE, pattern = "Complete"),
                         collar_type = "Telonics Iridium",
                         nskip = 23)
## subset columns for input data to collar-viewer app
locations <- gps[,c("collar_id","gps_fix_time","gps_latitude","gps_longitude","gps_satellite_count","gps_fix_attempt","schedule_set")]
names(locations) <- c("collar_id","fix_time","lat","lon","nsats","fix_type","fix_sched")
## load and compile site-specific capture Access dbs (currently maintained by J.Laufenberg, but eventually will directly pull from server)
file_names <- list.files("./data/raw_data/capture/oldversion_dbs", full.names=TRUE)
captures <- import_captures(file_names)
## screen capture data for errors
capture_errors <- qc_captures(captures)
clean_captures <- remove_capture_errors(captures, capture_errors)
## compile gps and capture data into single input data.frame for collar-viewer app
dat = make_cvdat(clean_captures, locations)
## import tpf file data and append fix rate values to input data.frame for collar-viewer app
tpfs <- list.files("./data/raw_data/gps_collar/telonics/telonics_tpf", pattern = "\\.tpf", full = TRUE)
fxr_scheds <- do.call("rbind",lapply(tpfs, import_tpf))
dat <- add.fixrates(dat, fxr_scheds)
## save R data object for upload to collar-viewer app
save(dat, file = "./data/derived_data/collar_data_test.RData")


