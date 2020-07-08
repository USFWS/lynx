
#' Merges Tables from Multiple MS Access Databases into a Single Dataframe
#'
#' Connects to a folder of MS Access databases, imports MS Access tables as dataframes, 
#' merges these dataframes into a single dataframe, and returns the merged dataframe.
#'
#' @keywords Access, table, import, merge
#' 
#' @param path A directory path to the folder containing the Access databases.
#' @param file_names The file names of the Access databases.
#' @param table_name The name of the table in the Access databasese to read in.
#'
#' @return A dataframe of the tbl_capture data from the lynx Access database
#'
#' @example
#' path <- paste0("s:/im_archive/boreal_lynx/incoming/capture_data/source/")
#' file_names <- c("lynx_capture_database_tet_20190503",  "lynx_capture_database_wise_20190701", "lynx_capture_database_kan_20190522", "lynx_capture_database_ykf_20190426")
#' table_name <- "tbl_captures"
#' dat <- access_merge(path, file_names, table_name)
#' 
access_merge <- function(path, file_names, table_name){
  require(odbc)
  require(tidyverse)
  
  # Create a list of connections to the Access databases and read in the capture tables:
  dat <- lapply(file_names, function(x) {
    access_files <- paste0(path, x, ".accdb")
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",access_files,";"))
    DBI::dbReadTable(con, table_name)
  })
  # Merge the list of capture dataframes into a single df:
  dat <- do.call(rbind, dat)
}
