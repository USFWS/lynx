


library(RODBC)
source("./R/refactor.R")

## Create a list of connections to the Access databases and read in the capture tables:

file.names <- list.files("./data/raw_data/capture/test_dbs", full.names=TRUE)
##file_name <- file_names[1]
##file_name <- "./data/raw_data/capture/db_development/test_lynx_capture_database_20200317-02.accdb"
##file_name <- "./data/raw_data/capture/db_development/test_lynx_capture_database_202000714-01.accdb"

##myfunc <- function(file_name){

get.access.db <- function(x, file_name) {
    con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",file_name,";"))
    df <- sqlFetch(con, x)
    odbcClose(con)
    return(df)
}
table_names <- c("fates", "mort_sites", "necropsies", "captures")


##------- Future extraction of study site ---------##
siteids <- c("kan", "kuk", "tet", "wsm", "ykf")
sites <- unlist(sapply(siteids, grep, x = file_names))
indx <- regexpr("database_", file_names)
cap.site <- toupper(siteids)[which(siteids == substr(file_names, indx + 9, indx + 11))]
cap.site <- "TET"
##-------------------------------------------------##

cap.tables <- lapply(file_names, getdb, x = "captures")
names(cap.tables) <- names(sites)


cap.dat <- do.call("rbind", mapply(function(table, site){
    table$Site = toupper(site )
    return(table)
    },
    table = cap.tables, site = names(sites), SIMPLIFY = FALSE))

#dbtables <- mapply(getdb, x = table_names, MoreArgs = list(file_name = file_name))
##dbtables <- getdb("captures", file_name)
#names(dbtables) <- table_names
##str(dbtables)

col.keep <- c("Capture_Date", "Lynx_ID", "Site", "Den_ID", "Sex", "Age", "Ear_Tag_Left",
              "Ear_Tag_Right", "Collar_Removed", "Removed_Collar_Make", "Removed_Collar_Model",
              "Removed_Collar_SN", "Removed_Collar_Freq", "Capture_UTMe", "Capture_UTMn", "Capture_Lon",
              "Capture_Lat", "Recapture", "Collar_Deployed", "Collar_Make", "Collar_Model", "Collar_SN",
              "Frequency", "Anaesthetized")
cap.dat <- cap.dat[is.na(cap.dat$Den_ID),col.keep] # remove records w/ den IDs (newborn kittens)
cap.dat <- cap.dat[cap.dat$Removed_Collar_Make %in% "Telonics" | cap.dat$Collar_Make %in% "Telonics",] # restrict to Telonics collars
cap.dat <- refactor(cap.dat[order(cap.dat$Capture_Date),])
cap.dat$Removed_Collar_SN <- sub("A","",cap.dat$Removed_Collar_SN) # remove "A" character from collar SNs
cap.dat$Collar_SN <- sub("A","",cap.dat$Collar_SN) # remove "A" character from collar SNs
dim(cap.dat)





col.keep <- c("Lynx_ID", "Site", "Capture_Date")
test.track <- data.frame("Capture_Date" = .POSIXct(character()), "Lynx_ID" = factor())
## missing lynx ID
test <- cap.dat[is.na(cap.dat$Lynx_ID),]
if(nrow(test) != 0){
    warning("Missing Lynx_ID:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## Incorrect site code in lynx ID
test <- cap.dat[!(substr(cap.dat$Lynx_ID, 1, 3) == cap.dat$Site),]
if(nrow(test) != 0){
    warning("Incorrect site code used in Lynx_ID:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## Incorrect character length for lynx ID
test <- cap.dat[nchar(as.character(cap.dat$Lynx_ID)) != 6,]
if(nrow(test) != 0){
    warning("Incorrect number of characters used for Lynx_ID:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## Incorrect individual code in lynx ID
test <- cap.dat[is.na(as.numeric(substr(cap.dat$Lynx_ID, 4, 6))),]
if(nrow(test) != 0){
    warning("Incorrect individual code used in Lynx_ID:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## No collar deployed/no recapture/specs recorded for deployed collar
test <- cap.dat[which(cap.dat$Collar_Deployed  ==  "N" & cap.dat$Recapture == "N" &
                          apply(cap.dat[,c("Collar_Make","Collar_Model","Collar_SN","Frequency")], 1,
                                function(x)any(!is.na(x)))),]
if(nrow(test) != 0){
    warning("Data recorded for deployed collar when Collar_Deployed and Recapture recorded as N:")
    print(test[,c(col.keep,"Collar_Deployed","Recapture","Collar_Make", "Collar_Model","Collar_SN","Frequency")])
    test.track <- rbind(test.track, test)
}
## Collared lynx recaptured/released w/ same collar?
test <- cap.dat[which(cap.dat$Collar_Deployed == "N" & cap.dat$Recapture == "Y" &
                          apply(cap.dat[,c("Collar_Make","Collar_Model","Collar_SN","Frequency")], 1,
                                function(x)any(!is.na(x)))),]
if(nrow(test) != 0){
    warning("Verify individual captured and released with same collar:")
    print(test[,c(col.keep,"Collar_Deployed","Recapture","Collar_Make", "Collar_Model","Collar_SN","Frequency")])
}
## collar deployed/no collar serial number
test <- cap.dat[which(cap.dat$Collar_Deployed == "Y" & is.na(cap.dat$Collar_SN)),]
if(nrow(test) != 0){
    warning("Missing serial number for deployed collar:")
    print(test[,c(col.keep,"Collar_Deployed","Collar_SN")])
    test.track <- rbind(test.track, test)
}
## collar deployed/no collar make
test <- cap.dat[which(cap.dat$Collar_Deployed == "Y" & is.na(cap.dat$Collar_Make)),]
if(nrow(test) != 0){
    warning("Missing make for deployed collar:")
    print(test[,c(col.keep,"Collar_Deployed","Collar_Make")])
    test.track <- rbind(test.track, test)
}
## collar deployed/no anaesthesia
test <- cap.dat[which(cap.dat$Collar_Deployed == "Y" & cap.dat$Anaesthetized == "N"),]
if(nrow(test) != 0){
    warning("Collar deployed without anaesthesia:")
    print(test[,c(col.keep,"Collar_Deployed","Anaesthetized")])
    test.track <- rbind(test.track, test)
}
## No collar removed/specs recorded for removed collar
test <- cap.dat[which(cap.dat$Collar_Removed == "N" &
                          apply(cap.dat[,c("Removed_Collar_Make","Removed_Collar_Model",
                                           "Removed_Collar_SN","Removed_Collar_Freq")], 1,
                                function(x)any(!is.na(x)))),]
if(nrow(test) != 0){
    warning("Data recorded for removed collar when Collar_Removed recorded as N:")
    print(test[,c(col.keep,"Collar_Removed","Removed_Collar_Make","Removed_Collar_Model",
                  "Removed_Collar_SN", "Removed_Collar_Freq")])
    test.track <- rbind(test.track, test)
}
## collar removed/no collar serial number
test <- cap.dat[which(cap.dat$Collar_Removed == "Y" & is.na(cap.dat$Removed_Collar_SN)),]
if(nrow(test) != 0){
    warning("Missing serial number for removed collar:")
    print(test[,c(col.keep,"Collar_Removed","Removed_Collar_SN")])
}
## collar removed/no collar make
test <- cap.dat[which(cap.dat$Collar_Removed == "Y" & is.na(cap.dat$Removed_Collar_Make)),]
if(nrow(test) != 0){
    warning("Missing make for removed collar:")
    print(test[,c(col.keep,"Collar_Removed","Removed_Collar_Make")])
}
## collar removed/no anaesthesia
test <- cap.dat[which(cap.dat$Collar_Removed == "Y" & cap.dat$Anaesthetized == "N"),]
if(nrow(test) != 0){
    warning("Collar removed without anaesthesia:")
    print(test[,c(col.keep,"Collar_Removed","Anaesthetized")])
}
## collar removed/not recapture
test <- cap.dat[cap.dat$Collar_Removed == "Y" & cap.dat$Recapture == "N",]
if(nrow(test) != 0){
    warning("Collar removed when Recapture recorded as N:")
    print(test[,c(col.keep,"Collar_Removed","Recapture")])
}
## missing capture date
test <- cap.dat[is.na(cap.dat$Capture_Date),]
if(nrow(test) != 0){
    warning("Missing Capture_Date:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## missing sex
test <- cap.dat[is.na(cap.dat$Sex),]
if(nrow(test) != 0){
    warning("Missing Sex:")
    print(test[,c(col.keep,"Sex")])
    test.track <- rbind(test.track, test)
}
## missing age
test <- cap.dat[is.na(cap.dat$Age),]
if(nrow(test) != 0){
    warning("Missing Age:")
    print(test[,c(col.keep,"Age")])
    test.track <- rbind(test.track, test)
}
## missing capture location data
test <- cap.dat[apply(cap.dat[,c("Capture_UTMe","Capture_UTMn","Capture_Lon","Capture_Lat")], 1,
                function(x)all(is.na(x))),]
if(nrow(test) != 0){
    warning("Missing capture location data:")
    print(test[,c(col.keep,"Capture_UTMe","Capture_UTMn","Capture_Lon","Capture_Lat")])
}



## collars recorded for multiple individuals
find.octc <- function(x){
    tmp = x
    tmp$Collar_Action = "deployed"
    col.keep = c("Capture_Date","Lynx_ID","Sex","Age","Collar_Make","Collar_SN","Site","Collar_Action")
    tmp = tmp[tmp$Collar_Make %in% "Telonics", col.keep]
    col.keep = c("Capture_Date","Lynx_ID","Sex","Age","Removed_Collar_Make","Removed_Collar_SN","Site")
    rem = x[x$Removed_Collar_Make %in% "Telonics", col.keep]
    colnames(rem)[colnames(rem) %in% c("Removed_Collar_Make","Removed_Collar_SN")] = c("Collar_Make","Collar_SN")
    if(nrow(rem) != 0){
        rem$Collar_Action = "removed"
        tmp = rbind(rem, tmp)
    }
    tmp = tmp[order(tmp$Capture_Date),]
    tmp$Collar_SN <- as.factor(tmp$Collar_SN)
    tmpl <- split(tmp, tmp$Collar_SN)
    indz <- which(sapply(tmpl, function(x)length(unique(x$Lynx_ID))) != 1)
    if(length(indz) != 0){warning("Collars recorded as deployed on >1 individual:")
        print(tmpl[indz])
    }
}

octc <- find.octc(cap.dat)






fate.dat <- refactor(dbtables$fates)
str(fate.dat)
dim(fate.dat)

#col.keep <- c("Fate_Date","Lynx_ID")
fate.errs <- fate.dat[0,]
fate.errs$error <- character()
errors <- c("missing lynx id", "incorrect site code", "incorrect lynx id length",
            "incorrect individual lynx id code", "missing fate date", "missing sex", "missing age class")
## missing lynx ID
test <- fate.dat[is.na(fate.dat$Lynx_ID),]
if(nrow(test) != 0){
    test$error <- "missing lynx id"
    warning("Missing Lynx_ID:")
    print(test)
    fate.errs <- rbind(fate.errs, test)
}
## Incorrect site code in lynx ID
test <- fate.dat[!(substr(fate.dat$Lynx_ID, 1, 3) == cap.site),]
if(nrow(test) != 0){
    test$error <- "incorrect site code"
    warning("Incorrect site code used in Lynx_ID:")
    print(test)
    fate.errs <- rbind(fate.errs, test)
}
## Incorrect character length for lynx ID
test <- fate.dat[nchar(as.character(fate.dat$Lynx_ID)) != 6,]
if(nrow(test) != 0){
    test$error <- "incorrect lynx id length"
    warning("Incorrect number of characters used for Lynx_ID:")
    print(test)
    fate.errs <- rbind(fate.errs, test)
}
## Incorrect individual code in lynx ID
test <- fate.dat[is.na(as.numeric(substr(fate.dat$Lynx_ID, 4, 6))),]
if(nrow(test) != 0){
    test$error <- "incorrect individual lynx id code"
    warning("Incorrect individual code used in Lynx_ID:")
    print(test)
    fate.errs <- rbind(fate.errs, test)
}
## missing fate date
test <- fate.dat[is.na(fate.dat$Fate_Date),]
if(nrow(test) != 0){
    test$error <- "missing fate date"
    warning("Missing Fate_Date:")
    print(test)
    fate.errs <- rbind(fate.errs, test)
}
## missing sex
test <- fate.dat[is.na(fate.dat$Sex),]
if(nrow(test) != 0){
    test$error <- "missing sex"
    warning("Missing Sex:")
    print(test[,c(col.keep,"Sex")])
    fate.errs <- rbind(fate.errs, test)
}
## missing age class
test <- fate.dat[is.na(fate.dat$Age_Class),]
if(nrow(test) != 0){
    test$error <- "missing age class"
    warning("Missing Age Class:")
    print(test[,c(col.keep,"Age")])
    fate.errs <- rbind(fate.errs, test)
}
fate.errs$error <- factor(fate.errs$error, levels = errors)


rl <- unique(c(sapply(list(do.call("rbind", xtmpl[indz])$Lynx_ID), unique),
               unique(as.character(test.track$Lynx_ID))))
x <- refactor(x[!x$Lynx_ID %in% rl,])
xlist <- split(x, x$Lynx_ID)
cl <- names(xlist)
str(xlist,max.level=1)

flist <- split(fate.dat, fate.dat$Lynx_ID)
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













