


library(RODBC)
source("./functions/refactor.R")

## Create a list of connections to the Access databases and read in the capture tables:

##file_names <- list.files("../data/raw_data/capture/working_databases", full.names=TRUE)
##file_name <- file_names[1]
file_name <- "../data/raw_data/capture/db_development/test_lynx_capture_database_20200317-02.accdb"

##myfunc <- function(file_name){

getdb <- function(x, file_name) {
    con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",file_name,";"))
    df <- sqlFetch(con, x)
    odbcClose(con)
    return(df)
}
table_names <- c("fates", "mort_sites", "necropsies", "tbl_captures")

##------- Future extraction of study site ---------##
siteids <- c("kan", "kuk", "tet", "wsm", "ykf")
indx <- regexpr("database_",file_name)
##cap.site <- toupper(siteids)[which(siteids==substr(file_name, indx + 9, indx + 11))]
cap.site <- "TET"
##-------------------------------------------------##


dat <- mapply(getdb, x = table_names, MoreArgs = list(file_name = file_name))
##dat <- getdb("tbl_captures", file_name)
names(dat) <- table_names
##str(dat)

col.keep <- c("Capture_Date", "Lynx_ID", "Den_ID", "Sex", "Age", "Ear_Tag_Left",
              "Ear_Tag_Right", "Collar_Removed", "Removed_Collar_Make", "Removed_Collar_Model",
              "Removed_Collar_SN", "Removed_Collar_Freq", "Capture_UTMe", "Capture_UTMn", "Capture_Lon",
              "Capture_Lat", "Recapture", "Collar_Deployed", "Collar_Make", "Collar_Model", "Collar_SN",
              "Frequency", "Anaesthetized")
x <- dat$tbl_captures[is.na(dat$tbl_captures$Den_ID),col.keep]
x <- x[x$Removed_Collar_Make%in%"Telonics" | x$Collar_Make%in%"Telonics",]
x <- refactor(x[order(x$Capture_Date),])
x$Site <- cap.site
x$Removed_Collar_SN <- sub("A","",x$Removed_Collar_SN)
x$Collar_SN <- sub("A","",x$Collar_SN)
dim(x)

col.keep <- c("Capture_Date","Lynx_ID")
test.track <- data.frame("Capture_Date" = .POSIXct(character()), "Lynx_ID" = factor())
## Incorrect site code in lynx ID
test <- x[!(substr(x$Lynx_ID, 1, 3)==cap.site),]
if(nrow(test)!=0){warning("Incorrect site code used in Lynx_ID:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## Incorrect character length for lynx ID
test <- x[nchar(as.character(x$Lynx_ID))!=6,]
if(nrow(test)!=0){warning("Incorrect number of characters used for Lynx_ID:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## Incorrect individual code in lynx ID
test <- x[is.na(as.numeric(x$Lynx_ID, 4, 6)),]
if(nrow(test)!=0){warning("Incorrect individual code used in Lynx_ID:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## No collar deployed/no recapture/specs recorded for deployed collar
test <- x[which(x$Collar_Deployed=="N" & x$Recapture=="N" &
                apply(x[,c("Collar_Make","Collar_Model","Collar_SN","Frequency")], 1,
                      function(x)any(!is.na(x)))),]
if(nrow(test)!=0){warning(paste0("Data recorded for deployed collar when Collar_Deployed",
                                 " and Recapture recorded as N:"))
                                 print(test[,c(col.keep,"Collar_Deployed","Recapture","Collar_Make",
                                               "Collar_Model","Collar_SN","Frequency")])
                                 test.track <- rbind(test.track, test)
}
## Collared lynx recaptured/released w/ same collar?
test <- x[which(x$Collar_Deployed=="N" & x$Recapture=="Y" &
                apply(x[,c("Collar_Make","Collar_Model","Collar_SN","Frequency")], 1,
                      function(x)any(!is.na(x)))),]
if(nrow(test)!=0){warning("Verify individual captured and released with same collar:")
    print(test[,c(col.keep,"Collar_Deployed","Recapture","Collar_Make",
                  "Collar_Model","Collar_SN","Frequency")])
}
## collar deployed/no collar serial number
test <- x[which(x$Collar_Deployed=="Y" & is.na(x$Collar_SN)),]
if(nrow(test)!=0){warning("Missing serial number for deployed collar:")
    print(test[,c(col.keep,"Collar_Deployed","Collar_SN")])
test.track <- rbind(test.track, test)
}
## collar deployed/no collar make
test <- x[which(x$Collar_Deployed=="Y" & is.na(x$Collar_Make)),]
if(nrow(test)!=0){warning("Missing make for deployed collar:")
    print(test[,c(col.keep,"Collar_Deployed","Collar_Make")])
test.track <- rbind(test.track, test)
}
## collar deployed/no anaesthesia
test <- x[which(x$Collar_Deployed=="Y" & x$Anaesthetized=="N"),]
if(nrow(test)!=0){warning("Collar deployed without anaesthesia:")
    print(test[,c(col.keep,"Collar_Deployed","Anaesthetized")])
test.track <- rbind(test.track, test)
}
## No collar removed/specs recorded for removed collar
test <- x[which(x$Collar_Removed=="N" & apply(x[,c("Removed_Collar_Make","Removed_Collar_Model",
                                                   "Removed_Collar_SN","Removed_Collar_Freq")], 1,
                                              function(x)any(!is.na(x)))),]
if(nrow(test)!=0){warning("Data recorded for removed collar when Collar_Removed recorded as N:")
    print(test[,c(col.keep,"Collar_Removed","Removed_Collar_Make","Removed_Collar_Model",
                  "Removed_Collar_SN", "Removed_Collar_Freq")])
    test.track <- rbind(test.track, test)
}
## collar removed/no collar serial number
test <- x[which(x$Collar_Removed=="Y" & is.na(x$Removed_Collar_SN)),]
if(nrow(test)!=0){warning("Missing serial number for removed collar:")
    print(test[,c(col.keep,"Collar_Removed","Removed_Collar_SN")])
}
## collar removed/no collar make
test <- x[which(x$Collar_Removed=="Y" & is.na(x$Removed_Collar_Make)),]
if(nrow(test)!=0){warning("Missing make for removed collar:")
    print(test[,c(col.keep,"Collar_Removed","Removed_Collar_Make")])
}
## collar removed/no anaesthesia
test <- x[which(x$Collar_Removed=="Y" & x$Anaesthetized=="N"),]
if(nrow(test)!=0){warning("Collar removed without anaesthesia:")
    print(test[,c(col.keep,"Collar_Removed","Anaesthetized")])
}
## collar removed/not recapture
test <- x[x$Collar_Removed=="Y" & x$Recapture=="N",]
if(nrow(test)!=0){warning("Collar removed when Recapture recorded as N:")
    print(test[,c(col.keep,"Collar_Removed","Recapture")])
}
## missing capture date
test <- x[is.na(x$Capture_Date),]
if(nrow(test)!=0){warning("Missing Capture_Date:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## missing lynx id
test <- x[is.na(x$Lynx_ID),]
if(nrow(test)!=0){warning("Missing Lynx_ID:")
    print(test[,col.keep])
    test.track <- rbind(test.track, test)
}
## missing sex
test <- x[is.na(x$Sex),]
if(nrow(test)!=0){warning("Missing Sex:")
    print(test[,c(col.keep,"Sex")])
    test.track <- rbind(test.track, test)
}
## missing age
test <- x[is.na(x$Age),]
if(nrow(test)!=0){warning("Missing Age:")
    print(test[,c(col.keep,"Age")])
    test.track <- rbind(test.track, test)
}
## missing capture location data
test <- x[apply(x[,c("Capture_UTMe","Capture_UTMn","Capture_Lon","Capture_Lat")], 1,
                function(x)all(is.na(x))),]
if(nrow(test)!=0){warning("Missing capture location data:")
    print(test[,c(col.keep,"Capture_UTMe","Capture_UTMn","Capture_Lon","Capture_Lat")])
}



## collars recorded for multiple individuals
col.keep <- c("Capture_Date","Lynx_ID","Sex","Age","Removed_Collar_Make","Removed_Collar_SN","Site")
xrem <- x[x$Removed_Collar_Make=="Telonics", col.keep]
xrem$ind <- 2
colnames(xrem)[colnames(xrem)%in%c("Removed_Collar_Make","Removed_Collar_SN")] <- c("Collar_Make","Collar_SN")
col.keep <- c("Capture_Date","Lynx_ID","Sex","Age","Collar_Make","Collar_SN","Site")
xdep <- x[x$Collar_Make=="Telonics", col.keep]
xdep$ind <- 1
xtmp <- rbind(xrem, xdep)
xtmp$Collar_SN <- as.factor(xtmp$Collar_SN)
xtmpl <- split(xtmp, xtmp$Collar_SN)
indz <- which(sapply(xtmpl, function(x)length(unique(x$Lynx_ID)))!=1)
if(length(indz)!=0){warning("Collars recorded as deployed on >1 individual:")
    print(xtmpl[indz])
}

rl <- unique(c(sapply(list(do.call("rbind", xtmpl[indz])$Lynx_ID), unique),
               unique(as.character(test.track$Lynx_ID))))
x <- refactor(x[!x$Lynx_ID %in% rl,])
xlist <- split(x, x$Lynx_ID)
cl <- names(xlist)
str(xlist,max.level=1)



comp <- function(x){
    tmp = x
    tmp$Collared_Status = 1
    col.keep = c("Capture_Date","Lynx_ID","Sex","Age","Collar_Make","Collar_SN","Site","Collared_Status")
    tmp = tmp[tmp$Collar_Make%in%"Telonics", col.keep]
    col.keep = c("Capture_Date","Lynx_ID","Sex","Age","Removed_Collar_Make","Removed_Collar_SN","Site")
    rem = x[x$Removed_Collar_Make %in% "Telonics", col.keep]
    colnames(rem)[colnames(rem) %in% c("Removed_Collar_Make","Removed_Collar_SN")] = c("Collar_Make","Collar_SN")
    if(nrow(rem)!=0){
        rem$Collared_Status = 2
        tmp = rbind(rem, tmp)
    }
    tmp = tmp[order(tmp$Capture_Date),]
    return(tmp)
}

rem <- collar.obs <- vector("list",length(xlist))
for(i in 1:length(xlist)){
    collar.obs[[i]] <- xlist[[i]]
    collar.obs[[i]]$Collared_Status <- 1
    col.keep <- c("Capture_Date","Lynx_ID","Sex","Age","Collar_Make","Collar_SN","Site","Collared_Status")
    collar.obs[[i]] <- collar.obs[[i]][collar.obs[[i]]$Collar_Make%in%"Telonics", col.keep]
#    collar.obs[[i]]$Collared_Status <- 1
    col.keep <- c("Capture_Date","Lynx_ID","Sex","Age","Removed_Collar_Make","Removed_Collar_SN","Site")
    rem[[i]] <- xlist[[i]][xlist[[i]]$Removed_Collar_Make%in%"Telonics", col.keep]
    colnames(rem[[i]])[colnames(rem[[i]])%in%c("Removed_Collar_Make",
                                               "Removed_Collar_SN")] <- c("Collar_Make","Collar_SN")
    if(nrow(rem[[i]])!=0){
        rem[[i]]$Collared_Status <- 2
        collar.obs[[i]] <- rbind(rem[[i]], collar.obs[[i]])
    }
    collar.obs[[i]] <- collar.obs[[i]][order(collar.obs[[i]]$Capture_Date),]
}


collar.obs <- lapply(xlist, comp)



fate.dat <- refactor(dat$fates[dat$fates$Lynx_Fate=="dead",])
str(fate.dat)
dim(fate.dat)


flist <- split(fate.dat, fate.dat$Lynx_ID)
dl <- names(flist)
dl <- dl[dl%in%cl]

myfunc <- function(dl, flist, c, tmp){
    tmp$Fate_Status <- "Live"
    if(c %in% dl){
        df <- flist[[which(dl==c)]]
        if(tmp$Collared_Status[nrow(tmp)]==2){
            tmp$Capture_Date[nrow(tmp)] <- df$Fate_Date
            tmp$Fate_Status[nrow(tmp)] <- "Dead"
            }
        if(tmp$Collared_Status[nrow(tmp)]==1){
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
lynx.table$Collared_Status <- ifelse(lynx.table$Collared_Status==1, "Collared", "Uncollared")

nl <- nrow(lynx.table)
gpsl <- vector("list", nl)
for(i in 1:nl){
    collars <- unique(ll[[i]]$Collar_SN)
    ncoll <- length(collars)
    gpsl[[i]] <- vector("list", ncoll)
    for(j in 1:length(collars)){
        gpsl[[i]][[j]] <- x[x$ctn%in%paste0(collars[j],"A"),]
        cut1 <- max(min(ll[[i]][ll[[i]]$Collar_SN%in%collars[j],"Capture_Date"])+(24*60*60),
                    min(gpsl[[i]][[j]]$fixtime))
        cut2 <- max(gpsl[[i]][[j]]$fixtime)
        if(any(ll[[i]][ll[[i]]$Collar_SN%in%collars[j],"Fate_Status"]%in%"Dead"))
            cut2 <- min(max(ll[[i]][ll[[i]]$Collar_SN%in%collars[j],"Capture_Date"]), max(gpsl[[i]][[j]]$fixtime))
        gpsl[[i]][[j]] <- gpsl[[i]][[j]][gpsl[[i]][[j]]$fixtime>cut1 & gpsl[[i]][[j]]$fixtime<cut2,]
    }
    gpsl[[i]] <- do.call("rbind",gpsl[[i]])
    gpsl[[i]]$lynx_id <- lynx.table[i,"Lynx_ID"]
    gpsl[[i]]$sex <- lynx.table[i,"Sex"]
    gpsl[[i]]$age <- lynx.table[i,"Age"]
    gpsl[[i]]$site <- lynx.table[i,"Site"]
}

str(refactor(do.call("rbind", gpsl)))













