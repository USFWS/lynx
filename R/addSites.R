
addSites <- function(collarIDs,CTNSiteRef){
    if(!all(collarIDs$CTN %in% CTNSiteRef$CTN)){
        stop("Not all CTNs referenced in TPFs are listed in CTN-Site reference table")
    }
    collarIDs$Site <- CTNSiteRef$Site[match(collarIDs$CTN,CTNSiteRef$CTN)]
    collarIDs
}
