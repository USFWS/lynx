
#' Import Tables from Access Database
#'
#' @param table_name Character vector of table names to be imported
#' @param full_file_path Directory path to Access database including file name
#'
#' @return Data.frame

import_table <- function(full_file_path, table_name) {
    con <- RODBC::odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                           full_file_path,";"))
    df <- RODBC::sqlFetch(con, table_name)
    RODBC::odbcClose(con)
    return(df)
}


#' Compile Capture Tables from Site-specific NWBF Lynx Access Databases
#'
#' @param file_names Character vector of full file paths to Access databases including file name
#' @param telonics Logical value indicating whether to only compile records for Telonics collars. Default is TRUE.
#' @param sites Character vector of site codes used to identify Access database file names to be used.
#'
#' @return A data.frame of compiled capture records.
#' @export
#'
#' @examples
#' \dontrun{
#' import_captures("tet_lynx_capture_database_202000101.accdb")}

import_captures <- function(file_names, telonics = TRUE, sites = c("kan", "kuk", "tet", "wsm", "ykf")){
  siteids <- unlist(sapply(sites, grep, x = file_names))
  cap_tables <- lapply(file_names, import_table, table_name = "captures")
  names(cap_tables) <- names(siteids)
  cap_dat <- do.call("rbind", mapply(function(table, site){
    table$Capture_Site = toupper(site )
    return(table)
  },
  table = cap_tables, site = names(siteids), SIMPLIFY = FALSE))
  cap_dat <- cap_dat[is.na(cap_dat$Den_ID),] # remove records w/ den IDs (newborn kittens)
  ## add "A" to Telonics collar IDs
  tel_obs = which(cap_dat$Removed_Collar_Make %in% "Telonics" | cap_dat$Collar_Make %in% "Telonics")
  collsn_indz = which(!is.na(cap_dat$Collar_SN[tel_obs]) & !grepl("A",cap_dat$Collar_SN[tel_obs]))
  cap_dat$Collar_SN[collsn_indz] = paste0(cap_dat$Collar_SN[collsn_indz], "A")
  remcollsn_indz = which(!is.na(cap_dat$Removed_Collar_SN[tel_obs]) & !grepl("A",cap_dat$Removed_Collar_SN[tel_obs]))
  cap_dat$Removed_Collar_SN[remcollsn_indz] = paste0(cap_dat$Removed_Collar_SN[remcollsn_indz], "A")
  ## remove non-Telonics collars
  if(telonics){
    cap_dat <- cap_dat[tel_obs,]
  }
  cap_dat <- refactor(cap_dat[order(cap_dat$Capture_Date),])
  return(cap_dat)
}
