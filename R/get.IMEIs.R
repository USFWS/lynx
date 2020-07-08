
get.IMEIs <- function(file){
    require(pdftools)
    myfunc <- function(sub){
        sub <- data.frame(sub)
        Iridiumcheck <- which(sub[,"text"]=="Iridium")
        IMEIcheck <- which(sub[Iridiumcheck+1,"text"]=="IMEI")
        IMEIs <- sub[Iridiumcheck[IMEIcheck]+3,"text"]
        IMEIs
    }
    rpdf <- pdf_data(file)
    indz <- sapply(rpdf,function(x)any(x[,"text"]== "Iridium"))
    subs <- rpdf[indz]
    unlist(sapply(subs,myfunc))
}
