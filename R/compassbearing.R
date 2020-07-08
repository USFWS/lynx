
compassbearing <- function(x){
    require(geosphere)
    compbear = bearing(x[1,], x[nrow(x),])
    if(sign(compbear)==-1){
        compbear = round(360+compbear,0)
    }else{
        compbear = round(compbear,0)
    }
    return(compbear)
}
