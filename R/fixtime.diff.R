
fixtime.diff <- function(x){
    c(as.numeric(difftime(x[-1],x[-length(x)],units="hours")),NA)
}
