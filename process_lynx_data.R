
## attach packages
library(AniTrackTools)
library(lynx)
## load and compile telonics csv file
gps_df <- import_TelCSVs("../directory_path", csv_pattern="Complete")
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
