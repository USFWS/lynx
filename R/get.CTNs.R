
get.CTNs <- function(file){
    require(pdftools)
    myfunc <- function(sub){
        sub <- data.frame(sub)
        CTNcheck <- which(sub[,"text"]=="CTN")
        Serialcheck <- which(sub[CTNcheck+1,"text"]=="(Serial")
        CTNs <- sub[CTNcheck[Serialcheck]+4,"text"]
        CTNs
    }
    rpdf <- pdf_data(file)
    indz <- sapply(rpdf,function(x)any(x[,"text"]== "CTN"))
    subs <- rpdf[indz]
    unlist(sapply(subs,myfunc))
}
