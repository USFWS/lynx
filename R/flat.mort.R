
flat.mort <- function(myfile){
    ## read in data as text and convert to data.frame
    m = readLines(myfile)
    ctn = substr(m[8],5,nchar(m[8]))
    m = read.csv(textConnection(paste0(m[-(1:23)],collapse="\n")),stringsAsFactors=FALSE)
    ## retain mortality data
    attr(m,"CTN") = ctn # assign CTN# as attribute
    if(all(is.na(m$GPS.Fix.Time))){
        m = list(NULL)
    }else{
        m = data.frame(id=rep(ctn,nrow(m)), fixtime=m[,"Acquisition.Time"], mort=m[,"Mortality"])
        if(all(is.na(m$mort))){
            m = list(NULL)
        }else{
            m = m[!m$mort=="",]
            ## Format time variables
            m$fixtime = as.POSIXct(round(as.POSIXct(m$fixtime, tz="UTC", format="%Y.%m.%d %H:%M:%S"),"mins"))
            if(nrow(m)==0)m = list(NULL)
        }
    }
    return(m)
}
