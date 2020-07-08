
cdist.fig <- function(cd=cd,ymax=ymax,figdir.path=figdir.path){
	ctn <- as.character(cd$id[1])
    png(paste0(figdir.path,"CTN",ctn,"_CumulStep.png"),
        width=6.5,height=6.5,units="in",res=192)
    cdist.plot(cd=cd,ymax=ymax)
    dev.off()
    cat("\\includegraphics[width=\\maxwidth]{figure/CTN",
        ctn,"_CumulStep.png}\n",sep="")
    cat("\\newline\n")
    cat("CTN ",ctn,": cumulative daily distance traveled;\n",
        "red symbols = 25-hr fix rate, blue symbols = 4-hr fix rate\n",sep="")
    cat("\\clearpage\n")
}

