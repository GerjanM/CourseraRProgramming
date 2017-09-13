rankall<-function(outcome,rank="best"){
    ## Read outcome data
    df<-read.csv("D:/Users/Gerjan/R/ProgramAssignment3data/outcome-of-care-measures.csv",colClasses="character")
    
    
    ## Check that state and outcome are valid
    ##check op state
    #statelist<-unique(df[,7])
    #e<-statelist[]==state
    #f<-statelist[e]
    #if(length(f)==1) print(" ") else stop("geen state", call. = TRUE)
    ##check op outcome
    trueoutcome<-c("heart attack","heart failure","pneumonia")
    g<-trueoutcome[]==outcome
    h<-trueoutcome[g]
    if(length(h)==1) print(" ") else stop("invalid outcome", call. = TRUE)

    
    ## For each state, find the hospital of the given rank
lijst<-df[df[7]=="AL",c(2,7,11,17,23)]
uniqueState<-unique(df[,7])

if(outcome=="heart failure"){
    hospitals<-lijst$Hospital.Name
    rijmortrates<-as.numeric(lijst$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    mrates<-cbind(hospitals,rijmortrates)
    cc<-complete.cases(mrates)
    mratesdef<-mrates[cc,]
    aantalrij<-nrow(mratesdef)
    ranking<-c(1:aantalrij)
    o<-order(rijmortrates,hospitals)
    lijstdef<-cbind(ranking,hospitals[o],rijmortrates[o])} else 
        
        if(outcome=="heart attack"){
            hospitals<-lijst$Hospital.Name
            rijmortrates<-as.numeric(lijst$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
            mrates<-cbind(hospitals,rijmortrates)
            cc<-complete.cases(mrates)
            mratesdef<-mrates[cc,]
            aantalrij<-nrow(mratesdef)
            ranking<-c(1:aantalrij)
            o<-order(rijmortrates,hospitals)
            lijstdef<-cbind(ranking,hospitals[o],rijmortrates[o])} else       
                
                if(outcome=="pneumonia"){
                    hospitals<-lijst$Hospital.Name
                    rijmortrates<-as.numeric(lijst$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                    mrates<-cbind(hospitals,rijmortrates)
                    cc<-complete.cases(mrates)
                    mratesdef<-mrates[cc,]
                    aantalrij<-nrow(mratesdef)
                    ranking<-c(1:aantalrij)
                    o<-order(rijmortrates,hospitals)
                    lijstdef<-cbind(ranking,hospitals[o],rijmortrates[o])}


    ## Return a data frame with the hospital names and the (abbreviated) state name
print(lijstdef)
if(rank=="best") {rank<-1} else
    if(rank=="worst") {rank<-aantalrij} else
        if(rank>aantalrij) {print(NA)}
print(lijstdef[rank,])

}