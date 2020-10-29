


#' Convert Lynx Capture Data to Animal Table for AniTrackTools
#'
#' @param caps A data.frame of lynx capture data imported from Access database(s)
#'
#' @return A data.frame formatted for use with AniTrackTools
#' @export


caps2animals <- function(caps){
  caps_names = c("Lynx_ID","Capture_Site","Capture_Date","Sex","Age","Collar_SN")
  att_names = c("animal_id","initial_release_site","initial_release_date","sex","age","collar_id")
  animals = caps[,names(caps) %in% caps_names,]
  animals = animals[,match(caps_names, names(animals))]
  names(animals) = att_names
  animals = animals[with(animals, order(animal_id, initial_release_date)),]
  animalsl = split(animals, animals$animal_id)
  collar_ids = sapply(animalsl, function(x){paste0(x$collar_id, collapse = ", ")})
  animals = animals[!duplicated(animals$animal_id),]
  animals$collar_ids = collar_ids#[match(animals$animal_id, names(collar_ids))]
  animals = animals[,!(names(animals) %in% "collar_id")]
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
  collars = caps[,names(caps) %in% caps_names]
  collars = collars[,match(caps_names, names(collars))]
  names(collars) = att_names
  collars = collars[!is.na(collars$collar_id),]
  collars = collars[with(collars, order(collar_id, deploy_date)),c("collar_id","animal_id","deploy_date","deploy_site")]
  collarsl = split(collars, collars$collar_id)
  animal_ids = sapply(collarsl, function(x){paste0(unique(x$animal_id), collapse = ", ")})
#  collars = collars[!duplicated(collars$collar_id),]
  collars$animal_ids = animal_ids[match(collars$collar_id, names(animal_ids))]
  rownames(collars) = NULL
  return(collars)
}




#' Create Collar-Viewer Input Data
#'
#' @param captures data.frame of capture data
#' @param locations gps data
#'
#' @return data.frame used as input for collar-viewer app
#' @export
#'

make_cvdat <- function(captures, locations){
  cond1 = captures$Collar_Make=="Telonics"
  cond2 = !is.na(captures$Collar_SN)
  ref = captures[cond1 & cond2,c("Collar_SN","Lynx_ID","Capture_Date")]
  names(ref) = c("collar_id", "animal_id", "deploy_date")
  ref$remove_date = as.POSIXct(NA)
  refl = split(ref, ref$collar_id)
  ref = do.call("rbind", lapply(refl, function(r){r[!duplicated(r$animal_id),]}))
  ##
  rem = captures[captures$Removed_Collar_Make=="Telonics" & !is.na(captures$Removed_Collar_SN), c("Removed_Collar_SN","Lynx_ID","Capture_Date")]
  names(rem) = c("collar_id", "animal_id", "remove_date")
  reml = split(rem, rem$collar_id)
  rem = do.call("rbind", lapply(reml, function(t){t[!duplicated(t$animal_id),]}))
  ##
  indz = match(paste0(rem$collar_id,rem$animal_id),paste0(ref$collar_id,ref$animal_id))
  indz = indz[!is.na(indz)]
  ref[indz,"remove_date"] = rem$remove_date
  ref = ref[with(ref,order(collar_id,deploy_date)),]
  ##
  for(i in 1:nrow(ref)){
    cond1 = locations$collar_id==ref[i,"collar_id"]
    cond2 = locations$fix_time > ref[i,"deploy_date"]
    cond3 = ifelse(!is.na(ref[i,"remove_date"]),
                   locations$fix_time < ref[i,"remove_date"],
                   rep(TRUE,nrow(locations)))
    locations$animal_id[cond1 & cond2 & cond3] = ref[i,"animal_id"]
  }
  ##
  dat = locations[!is.na(locations$animal_id),]
  animals = lynx::caps2animals(captures)
  dat$site = animals$initial_release_site[match(dat$animal_id, animals$animal_id)]
  dat$capture_date = animals$initial_release_date[match(dat$animal_id, animals$animal_id)]
  dat$age = animals$age[match(dat$animal_id, animals$animal_id)]
  dat$sex = animals$sex[match(dat$animal_id, animals$animal_id)]
  dat = dat[,c("animal_id","collar_id","site","fix_time","lat","lon","capture_date","sex","age","fix_type","fix_sched")]
  row.names(dat) = NULL
  return(dat)
  }





#' Add Fix Rates to Collar-Viewer Input Data
#'
#' @param dat Input data.frame for Collar-Viewer
#' @param fxr_scheds Table of program schedules for GPS collar
#'
#' @return Input data.frame for Collar-Viewer including fix rate values
#' @export

add_fixrates <- function(dat, fxr_scheds){
  names(fxr_scheds)[match(c("primary","aux_1","aux_2","aux_3"),names(fxr_scheds))] <- c("Primary","Auxiliary 1","Auxiliary 2","Auxiliary 3")
  dl = split(dat, dat$collar_id)
  for(i in 1:length(dl)){
    col_prog = fxr_scheds[fxr_scheds$ctn==dl[[i]]$collar_id[i],]
    fix_rate = unlist(col_prog[1,match(dl[[i]]$fix_sched,names(fxr_scheds))])
    names(fix_rate) = NULL
    dl[[i]]$fix_rate = fix_rate
  }
  dat = do.call("rbind", dl)
  return(dat)
}



