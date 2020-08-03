
## attach packages
library(AniTrackTools)
library(lynx)
## load and compile telonics csv file
files <- list.files("../directory_path", pattern="Complete", full=TRUE)
  gps_df <- do.call("rbind", lapply(files, import_telcsv, nskip = 23,
                                    fix_attempt_keep = c("Resolved QFP", "Resolved QFP (Uncertain)")))
## load and compile site-specific capture Access dbs
file_names <- list.files("../directory_path", full.names=TRUE)
captures <- import_captures(file_names)
## screen capture data for errors
capture_errors <- qc_captures(captures)
clean_captures <- remove_capture_errors(captures, capture_errors)
## create table of ancillary animal-specific data for collar-viewer app
animals <- caps2animals(clean_captures)
## format gps data for collar-viewer app
att_df = telcsv2ATT(gps_df,
                    data.frame(animal_id = clean_captures$Lynx_ID, collar_id = clean_captures$Collar_SN),
                    data.frame(animal_id = animals$animal_id, release_site = animals$release_site))
