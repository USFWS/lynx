
batch.flat.mort <- function(iridium_csv.dir=NULL,
                            save.file=TRUE,
                            save.dir=NULL,
                            returnx=FALSE,
                            pattern="Complete"){
    ## load custom function
    source("./functions/flat.mort.R")
    source("./functions/refactor.R")
    ## load GPS data file names
    files = unlist(lapply(iridium_csv.dir, function(x)list.files(x, pattern=pattern, full=TRUE)))
    ## number of GPS data files
    nfiles = length(files)
    ## process and aggregate all GPS data
    m <- do.call("rbind", lapply(files, flat.mort))
    m <- refactor(m)
    if(save.file){save(m, file=paste0(save.dir,"/flat.mort.gzip"))}
    if(returnx){return(m)}
}
