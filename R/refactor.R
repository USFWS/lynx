
refactor <- function(df){
    if(is.null(ncol(df))){
        if(is.factor(df)){out <- factor(df)}
    }else{
        for(i in 1:ncol(df)){
            if(!(is.factor(df[,i])))
                next
            df[,i] <- factor(df[,i])
        }
        out <- df
    }
    return(out)
}
