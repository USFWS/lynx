

#' Convert Lynx Capture Data to Animal Table for AniTrackTools
#'
#' @param caps A data.frame of lynx capture data imported from Access database(s)
#'
#' @return A data.frame formatted for use with AniTrackTools
#' @export


caps2animals <- function(caps){
  caps_names = c("Lynx_ID","Capture_Site","Capture_Date","Sex","Age","Collar_SN")
  att_names = c("animal_id","release_site","release_date","sex","age","collar_id")
  animals = caps[,names(caps) %in% caps_names,]
  animals = animals[,match(caps_names, names(animals))]
  names(animals) = att_names
  animals = animals[order(animals$release_date),]
  animalsl = split(animals, animals$animal_id)
  collar_ids = sapply(animalsl, function(x){paste0(x$collar_id, collapse = ", ")})
  animals = animals[!duplicated(animals$animal_id),]
  animals$collar_ids = collar_ids[match(animals$animal_id, names(collar_ids))]
  rownames(animals) = NULL
  return(animals)
}




#' Convert Lynx Capture Data to Collar Table for AniTrackTools
#'
#' @param caps A data.frame of lynx capture data imported from Access database(s)
#'
#' @return A data.frame formatted for use with AniTrackTools
#' @export


caps2collars <- function(caps){
  caps_names = c("Lynx_ID","Capture_Site","Capture_Date","Collar_SN")
  att_names = c("animal_id","deploy_site","deploy_date","collar_id")
  collars = caps[,names(caps) %in% caps_names,]
  collars = collars[,match(caps_names, names(collars))]
  names(collars) = att_names
  collars = collars[order(collars$deploy_date),c("collar_id","animal_id","deploy_date","deploy_site")]
  rownames(collars) = NULL
  return(collars)
}

