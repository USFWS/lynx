


#' Compile Capture Tables from Site-specific NWBF Lynx Access Databases
#'
#' @param file_names Character vector of full file paths to Access databases including file name
#' @param telonics Logical value indicating whether to only compile records for Telonics collars. Default is TRUE.
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
        table$Capture_Site = toupper(site )
        return(table)
    },
    table = cap_tables, site = names(siteids), SIMPLIFY = FALSE))
    cap_dat <- cap_dat[is.na(cap_dat$Den_ID),] # remove records w/ den IDs (newborn kittens)
    if(telonics){
    cap_dat <- cap_dat[cap_dat$Removed_Collar_Make %in% "Telonics" | cap_dat$Collar_Make %in% "Telonics",]
    }
    cap_dat <- refactor(cap_dat[order(cap_dat$Capture_Date),])
    return(cap_dat)
}



