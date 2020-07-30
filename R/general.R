

#' Recalculate Factor Levels Based on Unique Values
#'
#' @param df Data.frame
#'
#' @return Data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' df = data.frame(x = factor(LETTERS[1:5], levels = LETTERS[1:10]))
#' (length(unique(df$x))==nlevels(df$x))
#' df = refactor(df)
#' (length(unique(df$x))==nlevels(df$x))}

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

