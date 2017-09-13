best<-function(state,outcome){
    df<-read.csv("D:/Users/Gerjan/R/ProgramAssignment3data/outcome-of-care-measures.csv",colClasses="character")
    ##check op state
    statelist<-unique(df[,7])
    e<-statelist[]==state
    f<-statelist[e]
    if(length(f)==1) print(" ") else stop("geen state", call. = TRUE)
    
    ##check op outcome
    trueoutcome<-c("heart attack","heart failure","pneumonia")
    g<-trueoutcome[]==outcome
    h<-trueoutcome[g]
    if(length(h)==1) print(" ") else stop("invalid outcome", call. = TRUE)
    
    ## return hospital name in that state with loest 30-day feath rate
    if(outcome=="heart attack"){
        dfstate<-df[df[,7]==state,]
        dhamin<-min(as.numeric(dfstate[,11]),na.rm = TRUE)
        print(sort(dfstate[as.numeric(dfstate[,11])==dhamin,2]))} else{
    if(outcome == "heart failure"){
        dfstate<-df[df[,7]==state,]
        dhfmin<-min(as.numeric(dfstate[,17]),na.rm = TRUE)
        print(sort(dfstate[as.numeric(dfstate[,17])==dhfmin,2]))} else{
    if(outcome == "pneumonia"){
        dfstate<-df[df[,7]==state,]
        dpnmin<-min(as.numeric(dfstate[,23]),na.rm = TRUE)
        print(sort(dfstate[as.numeric(dfstate[,23])==dpnmin,2]))} else{
    }
    }
    }
}

    
    
best("TX", "heart attack")    
best("TX", "heart failure")   
best("MD", "heart attack")    
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")


best<-function(state,outcome){
    df<-read.csv("D:/Users/Gerjan/R/ProgramAssignment3data/outcome-of-care-measures.csv",colClasses="character")
    ##check op state
    statelist<-unique(df[,7])
    e<-statelist[]==state
    f<-statelist[e]
    if(length(f)==1) print(" ") else stop("geen state", call. = TRUE)
    
    ##check op outcome
    trueoutcome<-c("heart attack","heart failure","pneumonia")
    g<-trueoutcome[]==outcome
    h<-trueoutcome[g]
    if(length(h)==1) print(" ") else stop("invalid outcome", call. = TRUE)
    
    ## return hospital name in that state with lowest 30-day feath rate
    dfstate<-df[df[,7]==state,]
    if(outcome=="heart attack"){
        dhamin<-min(as.numeric(dfstate[,11]),na.rm = TRUE)
        print(sort(dfstate[as.numeric(dfstate[,11])==dhamin,2]))} else
    if(outcome == "heart failure"){
        dhfmin<-min(as.numeric(dfstate[,17]),na.rm = TRUE)
        print(sort(dfstate[as.numeric(dfstate[,17])==dhfmin,2]))} else
    if(outcome == "pneumonia"){
        dpnmin<-min(as.numeric(dfstate[,23]),na.rm = TRUE)
        print(sort(dfstate[as.numeric(dfstate[,23])==dpnmin,2]))}
}

        
    

