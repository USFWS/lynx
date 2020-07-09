
flat.gps <- function(myfile, collprogsfile="../data/raw_data/gps_collar/telonics/telonics_collar_programs.csv"){
    ## load required packages
    ##require(moveHMM)
    ## Create some bookkeeping objects
    levs1 = c("Resolved QFP", "Resolved QFP (Uncertain)",
              "Unresolved QFP", "Missing") # types of GPS fixes
    nlevs1 = length(levs1)
    levs2 = c("Primary","Auxiliary 1","Auxiliary 2","Auxiliary 3") # fix-rate schedule names
    nlevs2 = length(levs2)
    levs3 = c("P","A1","A2","A3")
    levs4 = paste0(1:12,"W")
    ## load collar schedule reference data
    tcp <- read.csv(collprogsfile)
    colnames(tcp)[match(c("primary","aux1","aux2","aux3"), colnames(tcp))] <- levs2

    ## read in data as text and convert to data.frame
    x = readLines(myfile)
    ctn = substr(x[8],5,nchar(x[8]))
    ndel = as.numeric(substr(x[3],13,nchar(x[3])))-1
    x = read.csv(textConnection(paste0(x[-(1:ndel)],collapse="\n")),stringsAsFactors=FALSE)
    ## retain location data and convert to dataframe
    attr(x,"CTN") = ctn # assign CTN# as attribute
    x = cbind(rep(ctn,nrow(x)),
              x[,c("GPS.Fix.Time","GPS.Fix.Attempt","GPS.Latitude","GPS.Longitude","Schedule.Set",
                   "Mortality","Acquisition.Time")])
    dimnames(x)[[2]] = c("ctn","fixtime","fixtype","lat","lon","fixsched","fate","acqtime")
    ## Format time variable
    x$fixtime = as.POSIXct(round(as.POSIXct(x$fixtime, tz="UTC", format="%Y.%m.%d %H:%M:%S"),"mins"))
    x$acqtime = as.POSIXct(round(as.POSIXct(x$acqtime, tz="UTC", format="%Y.%m.%d %H:%M:%S"),"mins"))
    ## subset out fate (a.k.a, mortality) data
    f <- x
    f = f[!f$fate=="",]
    ## process GPS data
    x$fixtype[x$fixtype==""] = NA # replace empty values w/ NA
    ## Subset out location data based on desired fix rate(s)
    x = x[x$fixtype %in% levs1 & x$fixsched %in% levs2,] #exclude 'Succeeded' fix type and missing fix schedules
    x$fixtype = factor(x$fixtype,levels=levs1) # convert GPS fix type to factor
    x$fixsched = factor(x$fixsched,levels=levs2) # convert fix-rate schedule names to factor
    ## Remove duplicate fixes (added on 23112018)
    x <- x[!duplicated(x$fixtime),]
    ## Create fixrate variable
    x$fixrate <- as.numeric(tcp[tcp$ctn==ctn,levs2])[match(x$fixsched,names(tcp[tcp$ctn==ctn,levs2]))]
    if(1==2){
        if(nrow(x)==0){
            x = list(NULL)
        }else{
            ## assign fate value to each GPS location
            if(any(f$fate=="Yes")){
                ftime <- f$acqtime[which.min(!f$fate=="Yes")]
                if(x$fixtime[nrow(x)]>ftime){
                    diffs <- x$fixtime-ftime
                    x$fate[diffs<0] <- "alive"
                    x$fate[!diffs<0] <- "dead"
                    x$fate <- factor(x$fate, levels = c("alive","dead","unkown"))
                }else{
                    x$fate <- factor("alive", levels = c("alive","dead","unkown"))
                }
            }else{
                x$fate <- factor("alive", levels = c("alive","dead","unkown"))
            }
            x <- subset(x, select = -c(acqtime))
            x = split(x,cumsum(c(1,diff(x$fixrate)!=0)))
            for(i in 1:length(x)){
                names(x)[[i]] = paste0("FixPeriod",i)
                attr(x[[i]],"FixPeriod") = x[[i]]$fixrate[1]
                allft = seq(x[[i]]$fixtime[1],
                            x[[i]]$fixtime[nrow(x[[i]])],
                            by=x[[i]]$fixrate[1]*3600)
                miss = c(0,which(!allft %in% x[[i]]$fixtime))
                rownames(x[[i]]) = 1:nrow(x[[i]])
                if(length(miss)>1){
                    n = length(miss)-1
                    x[[i]] = rbind(x[[i]],data.frame(ctn = rep(ctn,n), fixtime = allft[miss],
                                                     fixtype = rep(levs1[5],n), lat = rep(NA,n),
                                                     lon = rep(NA,n), fixsched = rep(NA,n), fate = rep(NA,n),
                                                     fixrate = rep(NA,n)))
                    x[[i]] = x[[i]][order(x[[i]]$fixtime),]
                    rownames(x[[i]]) = 1:nrow(x[[i]])
                    ind = 1 + cumsum(is.na(x[[i]]$lat))
                    not.na = !is.na(x[[i]]$lat)
                    xlist = split(x[[i]][not.na,], ind[not.na])
                    x[[i]]$fixtype[which(is.na(x[[i]]$fixtype))] = levels(x[[i]]$fixtype)[4]
                    indz = which(is.na(x[[i]]$fixrate))
                    for(j in 1:length(indz)){
                        x[[i]]$fixrate[indz[j]] = x[[i]]$fixrate[indz[j]-1]
                        x[[i]]$fixsched[indz[j]] = x[[i]]$fixsched[indz[j]-1]
                    } #j
                } else {
                    n = 0
                } #ifelse
                x[[i]][x[[i]]$fixtype==levs1[3],c("lat","lon")] <- NA
                ##x[[i]] = cbind(x[[i]],
                ##               prepData(data.frame(ID=x[[i]]$ctn, x=x[[i]]$lon,
                ##                                   y=x[[i]]$lat), type="LL")[,c("step","angle")])
                ##x[[i]]$deploy.site = tcp[tcp$CTN==ctn,"Site"]
            } #i
        } #ifelse
        x = do.call("rbind",x)
    }
    return(x)
}
