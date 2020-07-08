
create.indv.plots <- function(HMMdata=NULL,morttime=NULL,daterange=NULL,ymax=3000,fixrate="4",
                              stepcut=stepcut,ma.order=ma.order,figdir.path=NULL,
                              startdate1=startdate1,startdate2=startdate2,
                              duration1=duration1,duration2=duration2,
                              makehmm=TRUE,makecd=TRUE,makeplots=FALSE,save.data=FALSE,myreturn="none"){
    if(makecd){
        cd <- cdist.calc(HMMdata=HMMdata,daterange=daterange,morttime=morttime)
        if(save.data){
            save(cd,file="../data/derived_data/gps_collar/telonics/cumuldisttrav.gzip")
        }
    }
    if(makehmm){
        hmm <- HMMdata[HMMdata$fixrate==fixrate,]
        if(save.data){
            save(hmm,file=paste0("../data/derived_data/gps_collar/telonics/HMMdata_",fixrate,"fixrate.gzip"))
        }
    }
    if(makeplots){
        cat("\\subsection{Collar CTN: ",as.character(hmm$ID[1]),
            " (",as.character(hmm$site[1]),")}\n", sep="")
        cdist.fig(cd=cd,ymax=ymax,figdir.path=figdir.path)
        step.fig(hmm=hmm,stepcut=stepcut,ma.order=ma.order,figdir.path=figdir.path,startdate=startdate1)
        angle.fig(hmm=hmm,stepcut=stepcut,ma.order=ma.order,figdir.path=figdir.path,startdate=startdate1,
                  duration=duration1)
        step.fig(hmm=hmm,stepcut=stepcut,ma.order=ma.order,figdir.path=figdir.path,startdate=startdate2)
        angle.fig(hmm=hmm,stepcut=stepcut,ma.order=ma.order,figdir.path=figdir.path,startdate=startdate2,
                  duration=duration2)
    }
    if(myreturn=="both" & makecd & makehmm){return(list(hmm,cd))}
    if(myreturn=="hmm" & makehmm){return(list(hmm))}
    if(myreturn=="cd" & makecd){return(list(cd))}
    if(myreturn=="none"){return(myreturn)}
}

