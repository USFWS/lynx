
check4out <- function(xy, fence, daysback = 7){
    if(daysback == "all"){
        y = xy[!is.na(xy$lat),]
        coordinates(y) = ~lon+lat
        proj4string(y) = CRS("+init=epsg:4326") #WGS84
        y <- spTransform(y, proj4string(fence))
        if(any(!gContains(fence,y,byid=TRUE))){
            out = "Yes"
        }else{
            out = NA
        }
    }else{
        if(is.numeric(daysback))  t1 = as.Date(attributes(xy)$lastdate)-daysback
        if(!(all(xy$date> t1))){
            if(t1 %in% xy$date){
                y = xy[min(which(xy$date==t1)):nrow(xy),]
            }
            if(!(t1 %in% xy$date) & any(xy$date < t1)){
                y = xy[max(which(xy$date< t1)):nrow(xy),]
            }
            y = y[!is.na(y$lat),]
            coordinates(y) = ~lon+lat
            proj4string(y) = CRS("+init=epsg:4326") #WGS84
            y <- spTransform(y, proj4string(fence))
            if(all(!gContains(fence,y,byid=TRUE))){
                out = "Yes"
            }else{
                out = NA
            }
        }else{
            out = NA
        }
    }
    return(out)
}
