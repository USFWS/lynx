
make.HMMdata <- function(x,fixtypes=c("Missing","Unresolved QFP")){
    require(moveHMM)
    subx <- x[which(!x$fixtype %in% fixtypes),]
    moveHMMdata = prepData(data.frame(ID=subx$id,
                                      x=subx$lon,
                                      y=subx$lat,
                                      fixtime=subx$fixtime,
                                      fixtype=subx$fixtype,
                                      fixrate=subx$fixrate,
                                      site=subx$deploy.site,
                                      date=subx$date,
                                      datefac=subx$datefac), type="LL")
    attributes(moveHMMdata$step) = list(units="km")
    attributes(moveHMMdata$angle) = list(units="radians")
    return(moveHMMdata)
}
