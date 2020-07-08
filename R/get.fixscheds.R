
get.fixscheds <- function(file){
    require(pdftools)
    rpdf <- pdf_data(file) # read pdf as list of tables; pages as elements
    fixscheds <- rep(4,4)
    rpdf1 <- rpdf[[min(which(sapply(rpdf,function(x)any(x[,"text"]=="interval"))))]] # extract page where the word
    ## interval first appears and fix rate for Primary fix schedule is provided
    fr25 <- c("1","Day","+","1","Hour") # character string vector for 25-hr fix rate as used for Primary schedule
    s1 <- min(which(rpdf1$text=="interval"))+2
    e1 <- s1+4
    if(all(rpdf1$text[s1:e1]==fr25))fixscheds[1] <- 25 # test for 25-hr fix rate and conditional value assignment
    ind2 <- min(which(sapply(rpdf,function(x)any(x[,"text"]=="Auxiliary")))) # number of page with first appearance
    ## of the word Auxiliary
    rpdf2 <- rpdf[[ind2]] # extract page with fix rate for Auxiliary 1 schedule
    fr25a <- c("QFP","every","25") # character string pattern for 1 way Telonics specifies 25-hr fix rate for
    ## for Auxiliary schedules
    fr25b <- c("24","+1","QFP/day,") # the other way 25-hr fix rate is specified
    fr25c <- c("24","+1QFP/day,","VHF") # and another way 25-hr fix rate is specified;IMO a Telonics typo
    auxtest <- rpdf2$text=="Auxiliary" # find all occurrences of Auxiliary on this page
    if(sum(auxtest)==3){ # if Auxiliary occurs 3 times, then fix rates for all 3 Auxiliary schedules on this page
        s234 <- which(auxtest)+3
        e234 <- s234+2
        for(i in 1:3){
            txt <- rpdf2$text[s234[i]:e234[i]]
            if(all(txt==fr25a) | all(txt==fr25b) | all(txt==fr25c)) fixscheds[i+1] <- 25
            }
    }else{
        ssub1 <- which(auxtest)+3
        esub1 <- ssub1+2
        for(i in 1:length(ssub1)){
            txt <- rpdf2$text[ssub1[i]:esub1[i]]
            if(all(txt==fr25a) | all(txt==fr25b) | all(txt==fr25c)) fixscheds[i+1] <- 25
        }
        rpdf3 <- rpdf[[ind2+1]]
        auxtest2 <- rpdf3$text=="Auxiliary"
        ssub2 <- which(auxtest2)[1:(3-length(ssub1))]+3
        esub2 <- ssub2+2
        for(i in 1:length(ssub2)){
            txt <- rpdf3$text[ssub2[i]:esub2[i]]
            if(all(txt==fr25a) | all(txt==fr25b) | all(txt==fr25c)) fixscheds[i+(4-length(ssub2))] <- 25
        }
    }
    fixscheds
}
