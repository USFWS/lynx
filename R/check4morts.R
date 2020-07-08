
check4morts <- function(x,as.char=TRUE,as.date=TRUE){
    if(any(x$mort=="Yes")){
        m1 <- x$fixtime[min(which(x$mort=="Yes"))]
        if(as.date){m1 <- as.Date(m1,tz="UTC")}
        if(as.char){m1 <- as.character(m1)}
    }else{
        m1 = as.POSIXct(NA)
    }
    return(m1)
}
