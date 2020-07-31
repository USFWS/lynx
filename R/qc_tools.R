

data_errors <- c(row_indx = character(0), error_message = character(0), error_code = character(0))
## missing lynx ID
error_message <- "Missing Lynx_ID"
test <- which(is.na(cap_dat$Lynx_ID))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
    data_errors <- rbind(data_errors, add)
}
## Incorrect site code in lynx ID
error_message <- "Incorrect site code used in Lynx_ID"
test <- which(!(substr(cap_dat$Lynx_ID, 1, 3) == cap_dat$Capture_Site))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
    data_errors <- rbind(data_errors, add)
}
## Incorrect character length for lynx ID
error_message <- "Incorrect number of characters used for Lynx_ID"
test <- which(nchar(as.character(cap_dat$Lynx_ID)) != 6)
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
    data_errors <- rbind(data_errors, add)
}
## Incorrect individual code in lynx ID
error_message <- "Incorrect individual code used in Lynx_ID"
test <- which(is.na(as.numeric(substr(cap_dat$Lynx_ID, 4, 6))))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
    data_errors <- rbind(data_errors, add)
}
## No collar deployed/no recapture/specs recorded for deployed collar
error_message <- "Data recorded for deployed collar when Collar_Deployed and Recapture recorded as N"
test <- which(cap_dat$Collar_Deployed  ==  "N" & cap_dat$Recapture == "N" &
                  apply(cap_dat[,c("Collar_Make","Collar_Model","Collar_SN","Frequency")], 1,
                        function(x)any(!is.na(x))))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
    data_errors <- rbind(data_errors, add)
}
## collar deployed/no collar serial number
error_message <- "Missing serial number for deployed collar"
test <- which(cap_dat$Collar_Deployed == "Y" & is.na(cap_dat$Collar_SN))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
    data_errors <- rbind(data_errors, add)
}
## collar deployed/no collar make
error_message <- "Missing make for deployed collar"
test <- which(cap_dat$Collar_Deployed == "Y" & is.na(cap_dat$Collar_Make))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
    data_errors <- rbind(data_errors, add)
}
## collar deployed/no anesthesia
error_message <- "Collar deployed without anaesthesia"
test <- which(cap_dat$Collar_Deployed == "Y" & cap_dat$Anaesthetized == "N")
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
    data_errors <- rbind(data_errors, add)
}
## No collar removed/specs recorded for removed collar
error_message <- "Data recorded for removed collar when Collar_Removed recorded as N"
test <- which(cap_dat$Collar_Removed == "N" &
                          apply(cap_dat[,c("Removed_Collar_Make","Removed_Collar_Model",
                                           "Removed_Collar_SN","Removed_Collar_Freq")], 1,
                                function(x)any(!is.na(x))))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
    data_errors <- rbind(data_errors, add)
}
## collar removed/no collar serial number
error_message <- "Missing serial number for removed collar"
test <- which(cap_dat$Collar_Removed == "Y" & is.na(cap_dat$Removed_Collar_SN))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "Red")
    data_errors <- rbind(data_errors, add)
}
## collar removed/no collar make
error_message <- "Missing make for removed collar"
test <- which(cap_dat$Collar_Removed == "Y" & is.na(cap_dat$Removed_Collar_Make))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
    data_errors <- rbind(data_errors, add)
}
## collar removed/no anesthesia
error_message <- "Collar removed without anaesthesia"
test <- which(cap_dat$Collar_Removed == "Y" & cap_dat$Anaesthetized == "N")
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
    data_errors <- rbind(data_errors, add)
}
## collar removed/not recapture
error_message <- "Collar removed when Recapture recorded as N"
test <- which(cap_dat$Collar_Removed == "Y" & cap_dat$Recapture == "N")
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "yellow")
    data_errors <- rbind(data_errors, add)
}
## missing capture date
error_message <- "Missing Capture_Date"
test <- which(is.na(cap_dat$Capture_Date))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
    data_errors <- rbind(data_errors, add)
}
## missing sex
error_message <- "Missing Sex"
test <- which(is.na(cap_dat$Sex))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
    data_errors <- rbind(data_errors, add)
}
## missing age
error_message <- "Missing Age"
test <- which(is.na(cap_dat$Age))
l <- length(test)
if(l != 0){
    add <- c(row_index = as.character(test), error_message = rep(error_message, l), error_code = "red")
    data_errors <- rbind(data_errors, add)
}
## missing capture location data
error_message <- "Missing capture location data"
test <- which(apply(cap_dat[,c("Capture_UTMe","Capture_UTMn","Capture_Lon","Capture_Lat")], 1,
                function(x)all(is.na(x))))
l <- length(test)
if(l != 0){
    add <- data.frame(row_index = as.character(test),
                      error_message = rep(error_message, l),
                      error_code = rep("yellow", l))
    data_errors <- rbind(data_errors, add)
}
## collars recorded for multiple individuals
octc <- find.octc(cap_dat)
if(nrow(octc) != 0){
    data_errors = rbind(data_errors, octc)
}



find.octc <- function(x){
    error_message = "Collar recorded as deployed on >1 individual"
    col_keep = c("Capture_Date","Lynx_ID","Sex","Age","Collar_Make","Collar_SN","Capture_Site")
    dep = x[!is.na(x$Collar_SN), col_keep]
    col_keep = c("Capture_Date","Lynx_ID","Sex","Age","Removed_Collar_Make","Removed_Collar_SN","Capture_Site")
    rem = x[!is.na(x$Removed_Collar_SN), col_keep]
    colnames(rem)[colnames(rem) %in% c("Removed_Collar_Make","Removed_Collar_SN")] = c("Collar_Make","Collar_SN")
    if(nrow(rem) != 0){
        tmp = rbind(dep, rem)
    }
    tmp = tmp[order(tmp$Capture_Date),]
    tmp$Collar_SN <- as.factor(tmp$Collar_SN)
    tmp <- split(tmp, tmp$Collar_SN)
    indz <- which(sapply(tmp, function(x)length(unique(x$Lynx_ID))) != 1)
    if(length(indz) != 0){
        rows = sapply(tmp[indz], rownames)
    }
    test = which(rownames(x) %in% rows)
    l <- length(test)
    if(l != 0){
        data_errors <- data.frame(row_index = as.character(test),
                          error_message = rep(error_message, l),
                          error_code = rep("red", l))
    }
    return(data_errors)
}

octc <- find.octc(cap_dat)
red_errors <- rbind(red_errors, octc)

collars_removed <- unique(c(red_errors$Collar_SN, red_errors$Removed_Collar_SN))
collars_removed <- collars_removed[!is.na(collars_removed)]
animals_removed <- unique(red_errors$Lynx_ID)


data_removed <- cap_dat[cap_dat$Lynx_ID %in% animals_removed |
                            cap_dat$Collar_SN %in% collars_removed |
                            cap_dat$Removed_Collar_SN %in% collars_removed,]

table(tmp$Lynx_ID)
max(table(tmp$Collar_SN))
max(table(tmp$Removed_Collar_SN))


cap_dat <- cap_dat[!rownames(cap_dat) %in% rownames(data_removed),]

save(cap_dat, file = "./data/derived_data/cap_dat.RData")

id_match <- table(cap_dat[!is.na(cap_dat$Collar_SN),c("Lynx_ID","Collar_SN")])
colnames(id_match) <- paste0(colnames(id_match), "A")



fate_dat <- refactor(dbtables$fates)
str(fate_dat)
dim(fate_dat)

#col_keep <- c("Fate_Date","Lynx_ID")
fate_errs <- fate_dat[0,]
fate_errs$error <- character()
errors <- c("missing lynx id", "incorrect site code", "incorrect lynx id length",
            "incorrect individual lynx id code", "missing fate date", "missing sex", "missing age class")
## missing lynx ID
test <- fate_dat[is.na(fate_dat$Lynx_ID),]
if(nrow(test) != 0){
    test$error <- "missing lynx id"
    warning("Missing Lynx_ID:")
    print(test)
    fate_errs <- rbind(fate_errs, test)
}
## Incorrect site code in lynx ID
test <- fate_dat[!(substr(fate_dat$Lynx_ID, 1, 3) == fate_dat$Site),]
if(nrow(test) != 0){
    test$error <- "incorrect site code"
    warning("Incorrect site code used in Lynx_ID:")
    print(test)
    fate_errs <- rbind(fate_errs, test)
}
## Incorrect character length for lynx ID
test <- fate_dat[nchar(as.character(fate_dat$Lynx_ID)) != 6,]
if(nrow(test) != 0){
    test$error <- "incorrect lynx id length"
    warning("Incorrect number of characters used for Lynx_ID:")
    print(test)
    fate_errs <- rbind(fate_errs, test)
}
## Incorrect individual code in lynx ID
test <- fate_dat[is.na(as.numeric(substr(fate_dat$Lynx_ID, 4, 6))),]
if(nrow(test) != 0){
    test$error <- "incorrect individual lynx id code"
    warning("Incorrect individual code used in Lynx_ID:")
    print(test)
    fate_errs <- rbind(fate_errs, test)
}
## missing fate date
test <- fate_dat[is.na(fate_dat$Fate_Date),]
if(nrow(test) != 0){
    test$error <- "missing fate date"
    warning("Missing Fate_Date:")
    print(test)
    fate_errs <- rbind(fate_errs, test)
}
## missing sex
test <- fate_dat[is.na(fate_dat$Sex),]
if(nrow(test) != 0){
    test$error <- "missing sex"
    warning("Missing Sex:")
    print(test[,c(col_keep,"Sex")])
    fate_errs <- rbind(fate_errs, test)
}
## missing age class
test <- fate_dat[is.na(fate_dat$Age_Class),]
if(nrow(test) != 0){
    test$error <- "missing age class"
    warning("Missing Age Class:")
    print(test[,c(col_keep,"Age")])
    fate_errs <- rbind(fate_errs, test)
}
fate_errs$error <- factor(fate_errs$error, levels = errors)


rl <- unique(c(sapply(list(do.call("rbind", xtmpl[indz])$Lynx_ID), unique),
               unique(as.character(test_track$Lynx_ID))))
x <- refactor(x[!x$Lynx_ID %in% rl,])
xlist <- split(x, x$Lynx_ID)
cl <- names(xlist)
str(xlist,max.level=1)

flist <- split(fate_dat, fate_dat$Lynx_ID)
dl <- names(flist)
dl <- dl[dl %in% cl]

myfunc <- function(dl, flist, c, tmp){
    tmp$Fate_Status <- "Live"
    if(c %in% dl){
        df <- flist[[which(dl == c)]]
        if(tmp$Collared_Status[nrow(tmp)] == 2){
            tmp$Capture_Date[nrow(tmp)] <- df$Fate_Date
            tmp$Fate_Status[nrow(tmp)] <- "Dead"
            }
        if(tmp$Collared_Status[nrow(tmp)] == 1){
            tmp <- rbind(tmp, tmp[nrow(tmp),])
            tmp[nrow(tmp), "Capture_Date"] <- df$Fate_Date
            tmp[nrow(tmp), "Collared_Status"] <- 2
            tmp$Fate_Status[nrow(tmp)] <- "Dead"
        }
    }
    return(tmp)
}






ll <- mapply(myfunc, c = cl, tmp = collar.obs, MoreArgs = list(dl = dl, flist = flist),SIMPLIFY=FALSE)





lynx.table <- do.call("rbind",lapply(ll, function(x)x[nrow(x),]))

lynx.table$Collars_Worn <- sapply(ll, function(x)paste0(unique(x$Collar_SN), collapse = ", "))
lynx.table$Collared_Status <- ifelse(lynx.table$Collared_Status == 1, "Collared", "Uncollared")

nl <- nrow(lynx.table)
gpsl <- vector("list", nl)
for(i in 1:nl){
    collars <- unique(ll[[i]]$Collar_SN)
    ncoll <- length(collars)
    gpsl[[i]] <- vector("list", ncoll)
    for(j in 1:length(collars)){
        gpsl[[i]][[j]] <- x[x$ctn %in% paste0(collars[j],"A"),]
        cut1 <- max(min(ll[[i]][ll[[i]]$Collar_SN %in% collars[j],"Capture_Date"])+(24*60*60),
                    min(gpsl[[i]][[j]]$fixtime))
        cut2 <- max(gpsl[[i]][[j]]$fixtime)
        if(any(ll[[i]][ll[[i]]$Collar_SN %in% collars[j],"Fate_Status"] %in% "Dead"))
            cut2 <- min(max(ll[[i]][ll[[i]]$Collar_SN %in% collars[j],"Capture_Date"]), max(gpsl[[i]][[j]]$fixtime))
        gpsl[[i]][[j]] <- gpsl[[i]][[j]][gpsl[[i]][[j]]$fixtime>cut1 & gpsl[[i]][[j]]$fixtime<cut2,]
    }
    gpsl[[i]] <- do.call("rbind",gpsl[[i]])
    gpsl[[i]]$lynx_id <- lynx.table[i,"Lynx_ID"]
    gpsl[[i]]$sex <- lynx.table[i,"Sex"]
    gpsl[[i]]$age <- lynx.table[i,"Age"]
    gpsl[[i]]$site <- lynx.table[i,"Site"]
}

str(refactor(do.call("rbind", gpsl)))













