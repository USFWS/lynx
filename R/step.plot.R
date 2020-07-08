
step.plot <- function(hmm=NULL,stepcut=5,ma.order=42,startdate="first"){
    is.POSIXct <- function(x){inherits(x, "POSIXct")}
    if(is.POSIXct(startdate))hmm <- hmm[which(match(startdate,hmm$fixtime)):nrow(hmm),]
    if(is.numeric(startdate) & startdate<nrow(hmm))hmm <- hmm[(nrow(hmm)-startdate):nrow(hmm),]
    npts <- nrow(hmm)
    par(mfrow=c(1,2))
    plot(hmm$step, type="h",
         main=paste0("Step lengths\n", "CTN: ",hmm$ID[1],"\n",
                     "Site: ",hmm$site[1],"\n",
                     "Fix rate = 1 fix/",hmm$fixrate[1],"hrs\n",
                     "No. of fixes = ", npts,"\n"),
         cex.main=0.75, xlab="Fix date",
         ylab="Step length (km)", ylim=c(0,stepcut), axes=FALSE)
    mytickindz = c(seq(1, npts, by=round(npts/5,1)),npts)
    myticklabs = base::as.Date(hmm$fixtime[mytickindz])
    axis(1, at = mytickindz, labels=FALSE)
    text(x=mytickindz, y=par("usr")[3]-0.5, adj = 1, labels=myticklabs ,
         xpd=TRUE, srt=45, cex = 0.5)
    axis(2,at=0:stepcut,labels=FALSE)
    mtext(0:stepcut, side=2, at=0:stepcut, line=1.25)
    if(npts > ma.order){
        points(ma.order:npts,rollapply(hmm$step, ma.order, mean, na.rm=TRUE),type="l", col="red", lwd=1.5)
    }
    hist(hmm$step[hmm$step<stepcut],
         xlim=c(0,stepcut), breaks=seq(0,stepcut,0.1),
         xlab="Step length (km)",
         main=paste0("Step lengths\n","CTN: ",hmm$id[1],"\n",
                     "Site: ",hmm$site[1],"\n","Fix rate = 1 fix/",hmm$fixrate[1],"hrs\n",
                     "No. of fixes = ",npts,"\n"),
         cex.main=0.75)
    mtext(paste0("Number of steps >",stepcut,"km = ",
                 sum(!hmm$step<stepcut,na.rm=TRUE)), side=4, cex=0.75)
}
