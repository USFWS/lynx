
#' Import Tables from Access Database
#'
#' @param table_name Character vector of table names to be imported
#' @param full_file_path Directory path to Access database including file name
#'
#' @return Data.frame

import_tables <- function(full_file_path, table_name) {
    con <- RODBC::odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                           full_file_path,";"))
    df <- RODBC::sqlFetch(con, table_name)
    RODBC::odbcClose(con)
    return(df)
}
