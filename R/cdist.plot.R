
cdist.plot <- function(cd,ymax=3000){
    mycols <- c("red","blue")[match(cd$fixrate,c(25,4))]
    mypch <- c(16,17)[match(cd$fixrate,c(25,4))]
    plot.default(cd$date,cd$cdist, type="p",pch=mypch, cex=0.5, col=mycols, xaxt="n",
                 ylab="Distance (km)", xlab="Date",
                 ylim=c(0,ymax),
                 main="Cumulative daily distance traveled (km)")
    ndays <- nrow(cd)
    mytickindz = c(seq(1, ndays, by=round(ndays/8,0)), ndays)
    myticklabs = cd$date[mytickindz]
    axis(1, at = myticklabs, labels=FALSE)
    text(x=myticklabs, y=par("usr")[3]-100, adj = 1, labels=myticklabs ,
         xpd=TRUE, srt=45, cex = 0.5)
}


