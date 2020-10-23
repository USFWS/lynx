

#' Qualit Control Check Capture Data
#'
#' @param captures A data.frame object created from \code{import.captures} function
#'
#' @return A data.frame containing row index, error message, and error code for capture records flagged with errors.
#' @export
#'
#' @examples
#' \dontrun{
#' captures <- import_captures("tet_lynx_capture_database_202000101.accdb")}
#' qc_captures(captures)}



qc_captures <- function(captures){
    reqcols = c("Lynx_ID","Capture_Site","Collar_Deployed","Recapture","Collar_Make","Collar_Model",
                "Collar_SN","Frequency","Anaesthetized","Collar_Removed","Removed_Collar_Make",
                "Removed_Collar_Model","Removed_Collar_SN","Removed_Collar_Freq","Recapture",
                "Capture_Date","Sex","Age","Capture_UTMe","Capture_UTMn","Capture_Lon","Capture_Lat")
    col_test = reqcols %in% names(captures)
    if(!all(col_test))
       stop(paste0("Warning: The following columns are missing from input:\n",
                   paste0(reqcols[!col_test], collapse = ", ")))
    data_errors = data.frame(row_indx = character(0), error_message = character(0), error_code = character(0))
    error_messages = character(0)
    ## missing lynx ID
    error_message = "Missing Lynx_ID"
    error_messages = c(error_messages, error_message)
    test = which(is.na(captures$Lynx_ID))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
        data_errors = rbind(data_errors, add)
    }
    ## Incorrect site code in lynx ID
    error_message = "Incorrect site code used in Lynx_ID"
    error_messages = c(error_messages, error_message)
    test = which(!(substr(captures$Lynx_ID, 1, 3) == captures$Capture_Site))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
        data_errors = rbind(data_errors, add)
    }
    ## Incorrect character length for lynx ID
    error_message = "Incorrect number of characters used for Lynx_ID"
    error_messages = c(error_messages, error_message)
    test = which(nchar(as.character(captures$Lynx_ID)) != 6)
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
        data_errors = rbind(data_errors, add)
    }
    ## Incorrect individual code in lynx ID
    error_message = "Incorrect individual code used in Lynx_ID"
    error_messages = c(error_messages, error_message)
    test = which(is.na(as.numeric(substr(captures$Lynx_ID, 4, 6))))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
        data_errors = rbind(data_errors, add)
    }
    ## No collar deployed/no recapture/specs recorded for deployed collar
    error_message = "Data recorded for deployed collar when Collar_Deployed and Recapture recorded as N"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Deployed  ==  "N" & captures$Recapture == "N" &
                      apply(captures[,c("Collar_Make","Collar_Model","Collar_SN","Frequency")], 1,
                            function(x)any(!is.na(x))))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
        data_errors = rbind(data_errors, add)
    }
    ## collar deployed/no collar serial number
    error_message = "Missing serial number for deployed collar"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Deployed == "Y" & is.na(captures$Collar_SN))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
        data_errors = rbind(data_errors, add)
    }
    ## collar deployed/no collar make
    error_message = "Missing make for deployed collar"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Deployed == "Y" & is.na(captures$Collar_Make))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
        data_errors = rbind(data_errors, add)
    }
    ## collar deployed/no anesthesia
    error_message = "Collar deployed without anaesthesia"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Deployed == "Y" & captures$Anaesthetized == "N")
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
        data_errors = rbind(data_errors, add)
    }
    ## No collar removed/specs recorded for removed collar
    error_message = "Data recorded for removed collar when Collar_Removed recorded as N"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Removed == "N" &
                      apply(captures[,c("Removed_Collar_Make","Removed_Collar_Model",
                                      "Removed_Collar_SN","Removed_Collar_Freq")], 1,
                            function(x)any(!is.na(x))))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
        data_errors = rbind(data_errors, add)
    }
    ## collar removed/no collar serial number
    error_message = "Missing serial number for removed collar"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Removed == "Y" & is.na(captures$Removed_Collar_SN))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "Red")
        data_errors = rbind(data_errors, add)
    }
    ## collar removed/no collar make
    error_message = "Missing make for removed collar"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Removed == "Y" & is.na(captures$Removed_Collar_Make))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
        data_errors = rbind(data_errors, add)
    }
    ## collar removed/no anesthesia
    error_message = "Collar removed without anaesthesia"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Removed == "Y" & captures$Anaesthetized == "N")
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
        data_errors = rbind(data_errors, add)
    }
    ## collar removed/not recapture
    error_message = "Collar removed when Recapture recorded as N"
    error_messages = c(error_messages, error_message)
    test = which(captures$Collar_Removed == "Y" & captures$Recapture == "N")
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
        data_errors = rbind(data_errors, add)
    }
    ## missing capture date
    error_message = "Missing Capture_Date"
    error_messages = c(error_messages, error_message)
    test = which(is.na(captures$Capture_Date))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
        data_errors = rbind(data_errors, add)
    }
    ## missing sex
    error_message = "Missing Sex"
    error_messages = c(error_messages, error_message)
    test = which(is.na(captures$Sex))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
        data_errors = rbind(data_errors, add)
    }
    ## missing age
    error_message = "Missing Age"
    error_messages = c(error_messages, error_message)
    test = which(is.na(captures$Age))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
        data_errors = rbind(data_errors, add)
    }
    ## missing capture location data
    error_message = "Missing capture location data"
    error_messages = c(error_messages, error_message)
    test = which(apply(captures[,c("Capture_UTMe","Capture_UTMn","Capture_Lon","Capture_Lat")], 1,
                        function(x)all(is.na(x))))
    l = length(test)
    if(l != 0){
        add = data.frame(row_index = as.character(test),
                          error_message = rep(error_message, l),
                          error_code = rep("yellow", l))
        data_errors = rbind(data_errors, add)
    }
    ## collars recorded for multiple individuals
    error_message = "Collar recorded as deployed on >1 individual"
    error_messages = c(error_messages, error_message)
    octc = find.octc(captures)
    if(nrow(octc) != 0){
        data_errors = rbind(data_errors, octc)
    }
    data_errors$row_index = as.numeric(data_errors$row_index)
    data_errors$error_message = factor(data_errors$error_message, levels = error_messages)
    data_errors$error_code = factor(data_errors$error_code, levels = c("red", "yellow"))
    data_errors = data_errors[order(data_errors$error_code),]
        return(data_errors)
    }


#' Helper Function to Find Collars Deployed on >1 Lynx
#'
#' @param captures A data.frame object created from \code{import.captures} function.
#'
#' @return  A data.frame containing row index, error message, and error code for capture records flagged with errors.
#'

find.octc <- function(captures){
    error_message = "Collar recorded as deployed on >1 individual"
    col_keep = c("Capture_Date","Lynx_ID","Sex","Age","Collar_Make","Collar_SN","Capture_Site")
    dep = captures[!is.na(captures$Collar_SN), col_keep]
    col_keep = c("Capture_Date","Lynx_ID","Sex","Age","Removed_Collar_Make","Removed_Collar_SN","Capture_Site")
    rem = captures[!is.na(captures$Removed_Collar_SN), col_keep]
    colnames(rem)[colnames(rem) %in% c("Removed_Collar_Make","Removed_Collar_SN")] = c("Collar_Make","Collar_SN")
    if(nrow(rem) != 0){
        tmp = rbind(dep, rem)
    }
    tmp = tmp[order(tmp$Capture_Date),]
    tmp$Collar_SN = as.factor(tmp$Collar_SN)
    tmp = split(tmp, tmp$Collar_SN)
    indz = which(sapply(tmp, function(x)length(unique(x$Lynx_ID))) != 1)
    tmp = tmp[indz]
    tmp = lapply(tmp,function(tt){
        test = sapply(split(tt,tt$Lynx_ID),nrow)
        if(!all(test==1) & all(test<3)){
            NULL
        }else{
            tt
        }
    })
    names(tmp) = NULL
    rows = row.names(do.call("rbind", tmp))
    test = which(rownames(captures) %in% rows)
    l = length(test)
    if(l != 0){
        data_errors = data.frame(row_index = as.character(test),
                          error_message = rep(error_message, l),
                          error_code = rep("red", l))
    }
    return(data_errors)
}


#' Remove Lynx and Collars with Red-Light Errors
#'
#' @param captures A data.frame object created from \code{import.captures} function.
#' @param capture_errors A data.frame object created from \code{qc_captures} function.
#'
#' @return A data.frame of capture records with "red-light" errors removed.
#' @export
#'
#' @examples
#' \dontrun{
#' captures <- import_captures(file_names)
#' capture_errors <- qc_captures(captures)
#' clean_captures <- remove_capture_errors(captures, capture_errors)}

remove_capture_errors <- function(captures, capture_errors){
    remrows = capture_errors$row_index[capture_errors$error_code == "red"]
    remcollars = as.character(unlist(captures[remrows, c("Collar_SN","Removed_Collar_SN")]))
    remlynx = captures$Lynx_ID[remrows]
    captures_keep = captures[!captures$Lynx_ID %in% remlynx |
                                 !captures$Collar_SN %in% remcollars |
                                 !captures$Removed_Collar_SN %in% remcollars,]
    return(captures_keep)
}















