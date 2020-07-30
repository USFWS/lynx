


#' Compile Capture Tables from Site-specific NWBF Lynx Access Databases
#'
#' @param file_names Character vector of full file paths to Access databases including file name
#' @param telonics Logical indicating whether to only compile records with Telonics collars deployed or removed. Default is TRUE.
#' @param sites Character vector of site codes used to identify Access database file names to be used.
#'
#' @return Data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_captables("tet_lynx_capture_database_202000101.accdb")}

get_captables <- function(file_names, telonics = TRUE, sites = c("kan", "kuk", "tet", "wsm", "ykf")){
    siteids <- unlist(sapply(sites, grep, x = file_names))
    cap_tables <- lapply(file_names, import_tables, table_name = "captures")
    names(cap_tables) <- names(siteids)
    cap_dat <- do.call("rbind", mapply(function(table, site){
        table$Site = toupper(site )
        return(table)
    },
    table = cap_tables, site = names(siteids), SIMPLIFY = FALSE))
    col_keep <- c("Capture_Date", "Lynx_ID", "Site", "Den_ID", "Sex", "Age", "Ear_Tag_Left",
                  "Ear_Tag_Right", "Collar_Removed", "Removed_Collar_Make", "Removed_Collar_Model",
                  "Removed_Collar_SN", "Removed_Collar_Freq", "Capture_UTMe", "Capture_UTMn", "Capture_Lon",
                  "Capture_Lat", "Recapture", "Collar_Deployed", "Collar_Make", "Collar_Model", "Collar_SN",
                  "Frequency", "Anaesthetized")
    cap_dat <- cap_dat[is.na(cap_dat$Den_ID),col_keep] # remove records w/ den IDs (newborn kittens)
    if(telonics){
    cap_dat <- cap_dat[cap_dat$Removed_Collar_Make %in% "Telonics" | cap_dat$Collar_Make %in% "Telonics",]
    }
    cap_dat <- refactor(cap_dat[order(cap_dat$Capture_Date),])
    ##cap_dat$Removed_Collar_SN <- sub("A","",cap_dat$Removed_Collar_SN) # remove "A" character from collar SNs
    ##cap_dat$Collar_SN <- sub("A","",cap_dat$Collar_SN) # remove "A" character from collar SNs
    cap_dat
}



