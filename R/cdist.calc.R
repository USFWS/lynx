
cdist.calc <- function(HMMdata, daterange, morttime){
    cd <- data.frame(id=HMMdata$ID[1],date=daterange,step=NA,cdist=NA,fixrate=NA)
    dayind <- match(as.character(unique(HMMdata$datefac)),daterange)
    cd$step[dayind] <- aggregate(HMMdata$step,by=list(date=HMMdata$datefac),FUN=sum)$x
    cd$fixrate[dayind] <- aggregate(HMMdata$fixrate,by=list(date=HMMdata$datefac),FUN=function(x){
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    })$x
    cd$cdist[!is.na(cd$step)] <- cumsum(cd$step[!is.na(cd$step)])
    if(!is.na(morttime) & as.Date(morttime) > max(as.Date(daterange))){
        morttime <- as.character(max(as.Date(daterange)))
    }
    if(!is.na(morttime) & as.Date(morttime) > min(as.Date(daterange))){
        cd[match(morttime,cd$date):nrow(cd), c("step","cdist","fixrate")] <- NA
    }
    return(cd)
}
