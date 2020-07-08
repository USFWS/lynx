
angle.plot <- function(hmm=NULL,stepcut=5,ma.order=42,startdate="first"){
    is.POSIXct <- function(x){inherits(x, "POSIXct")}
    if(is.POSIXct(startdate))hmm <- hmm[which(match(startdate,hmm$fixtime)):nrow(hmm),]
    if(is.numeric(startdate) & startdate<nrow(hmm))hmm <- hmm[(nrow(hmm)-startdate):nrow(hmm),]
    npts <- nrow(hmm)
    par(mfrow=c(1,2))
    plot(hmm$angle,type="h", main=paste0("Turning angles\n","CTN: ",hmm$id[1],"\n",
                                     "Fix rate = 1 fix/",hmm$fixrate[1],"hrs\n","Site: ",hmm$site[1],"\n",
                                     "No. of fixes = ",npts,"\n"),
         cex.main=0.75, xlab="Time series index",
         ylab="Turning angle (radians)", ylim=c(-pi,pi), xaxt="n", yaxt="n")
    mytickindz = c(seq(1, npts, by=round(npts/5,1)),npts)
    myticklabs = base::as.Date(hmm$fixtime[mytickindz])
    axis(1, at = mytickindz, labels=FALSE)
    text(x=mytickindz, y=par("usr")[3]-0.5, adj = 1, labels=myticklabs ,
         xpd=TRUE, srt=45, cex = 0.5)
    axis(2,at=seq(-pi,pi,pi/4),labels=c(expression(paste("-",pi)),
                                        expression(paste("-3",pi,"/4")),
                                        expression(paste("-",pi,"/2")),
                                        expression(paste("-",pi,"/4")),
                                        0,
                                        expression(paste(pi,"/4")),
                                        expression(paste(pi,"/2")),
                                        expression(paste("3",pi,"/4")),
                                        expression(paste(pi))),
         cex.axis=0.7)
    abline(h=c(-pi,0,pi),lty=2)
    hist(hmm$angle,xlim=c(-pi,pi),breaks=seq(-pi,pi,pi/4),axes=FALSE,
         xlab="Turning angle (radians)",  main=paste0("Turning angles\n","CTN: ",hmm$id[1],"\n",
                                                      "Site: ",hmm$site[1],"\n","Fix rate = 1 fix/",
                                                      hmm$fixrate[1],"hrs\n","No. of fixes = ",npts),
         cex.main=0.75)
    axis(2)
    axis(1,at=seq(-pi,pi,pi/4),labels=c(expression(paste("-",pi)),
                                        expression(paste("-3",pi,"/4")),
                                        expression(paste("-",pi,"/2")),
                                        expression(paste("-",pi,"/4")),
                                        0,
                                        expression(paste(pi,"/4")),
                                        expression(paste(pi,"/2")),
                                        expression(paste("3",pi,"/4")),
                                        expression(paste(pi))),
         cex.axis=0.7)
    return(NULL)
}
