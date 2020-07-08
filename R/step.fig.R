
step.fig <- function(hmm=hmm,stepcut=stepcut,ma.order=ma.order,startdate=startdate,figdir.path=figdir.path){
    is.POSIXct <- function(x){inherits(x, "POSIXct")}
	ctn <- as.character(hmm$ID[1])
	dur <- "All"
	if(is.POSIXct(startdate)){dur <- paste0("since",as.character(startdate))}
	if(is.numeric(startdate)){dur <- "recent"}
    png(paste0(figdir.path,"CTN",ctn,"_StepLengths_",dur,".png"),
        width=6.5,height=4,units="in",res=96)
    step.plot(hmm=hmm,stepcut=stepcut,ma.order=ma.order,startdate=startdate)
    dev.off()
    cat("\\includegraphics{figure/CTN",ctn,
        "_StepLengths_",dur,".png}\n",sep="")
    cat("\\newline\n")
}

