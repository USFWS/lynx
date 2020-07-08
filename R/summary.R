Mode <- function(x) {
  d <- density(x,na.rm=TRUE)
  d$x[which.max(d$y)]
} #fn

summary.fn	<- function(x=NULL){
    cn <- c("Mean","SD","Mode","0%","2.5%","25%","50%","75%","97.5%","100%")
    if(!is.null(x)){
        if(any(is.na(x))){
            cat("\n\nWarning: x contains missing values\n\n")
        } #if
        if(is.matrix(x)){
            y <- array(0L,c(dim(x)[2],10))
            for(i in 1:dim(x)[2]){
                y[i,] <- c(mean(x[,i],na.rm=TRUE), sd(x[,i],na.rm=TRUE), Mode(x[,i]),
                           quantile(x[,i], probs = c(0,0.025,0.25,0.50,0.75,0.975,1), na.rm=TRUE))
                dimnames(y)[[2]] <- cn
                dimnames(y)[[1]] <- dimnames(x)[[2]]
            } #i
        }else{
            y <- c(mean(x,na.rm=TRUE), sd(x,na.rm=TRUE), Mode(x),
                   quantile(x, probs = c(0,0.025,0.25,0.50,0.75,0.975,1), na.rm=TRUE))
            names(y) <- cn
        } #ifelse
        return(y)
    }
    if(is.null(x)){return(cn)}
}# fn
