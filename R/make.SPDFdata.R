
make.SPDFdata <- function(x,fixtypes=c("Missing","Unresolved QFP")){
    subx <- x[which(!x$fixtype %in% fixtypes),]
    coordinates(subx) = ~lon+lat
    proj4string(subx) = CRS("+init=epsg:4326") #WGS84
    return(subx)
}
