

#' Convert Lynx Capture Data to Animal Table for AniTrackTools
#'
#' @param caps A data.frame of lynx capture data imported from Access database(s)
#'
#' @return A data.frame formatted for use with AniTrackTools
#' @export
#'
#' @examples
#' \dontrun{
#' captures <- import_captures("tet_lynx_capture_database_202000101.accdb")
#' caps2animals(captures)}

caps2animals <- function(caps){
  caps_names = c("Lynx_ID","Capture_Site","Capture_Date","Sex","Age","Collar_SN")
  att_names = c("animal_id","release_site","release_date","sex","age","collar_id")
  caps = caps[,names(caps) %in% caps_names,]
  caps = caps[,match(caps_names, names(caps))]
  names(caps) = att_names
  caps = caps[order(caps$release_date),]
  capsl = split(caps, caps$animal_id)
  collar_ids = sapply(capsl, function(x){paste0(x$collar_id, collapse = ", ")})
  caps = caps[!duplicated(caps$animal_id),]
  caps$collar_id = collar_ids[match(caps$animal_id, names(collar_ids))]
  names(caps)[names(caps) == "collar_id"] = "collar_ids"
  return(caps)
}




