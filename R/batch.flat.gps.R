
batch.flat.gps <- function(iridium_csv.dir="s:/im_archive/boreal_lynx/products/iridium_csv",
                           save.file=TRUE,
                           save.dir=NULL,
                           returnx=FALSE,
                           pattern="Complete"){
    ## load custom function
    source("./functions/flat.gps.R")
    ## load GPS data file names
    files = unlist(lapply(iridium_csv.dir, function(x)list.files(x, pattern=pattern, full=TRUE)))
    ## number of GPS data files
    nfiles = length(files)
    ## extract all CTNs
    ## process and aggregate all GPS data
    ##    print(system.time({
    x <- do.call("rbind", lapply(files, flat.gps))
    ##    }))
    if(save.file){save(x, file=paste0(save.dir,"/flat.gps.gzip"))}
    if(returnx){return(x)}
}


xlist <- vector("list",nfiles)
for(i in 1:nfiles){
    xlist[[i]] <- flat.gps(files[i])
}
