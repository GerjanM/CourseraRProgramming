rankhospital<-function(state,outcome,rank){
## Read outcome data
df<-read.csv("D:/Users/Gerjan/R/ProgramAssignment3data/outcome-of-care-measures.csv",colClasses="character")
df<-read.csv("D:/Users/Gerjan/R/ProgramAssignment3data/outcome-of-care-measures.csv")
## Check that state and outcome are valid
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


## Return hospital name in that state with the given rank
## 30-day death rate

dfstate<-df[df[,7]==state,]
if(outcome=="heart failure"){
    lijst<-dfstate[,c(2,11,17,23)]
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
            lijst<-dfstate[,c(2,11)]
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
                    lijst<-dfstate[,c(2,23)]
                    hospitals<-lijst$Hospital.Name
                    rijmortrates<-as.numeric(lijst$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                    mrates<-cbind(hospitals,rijmortrates)
                    cc<-complete.cases(mrates)
                    mratesdef<-mrates[cc,]
                    aantalrij<-nrow(mratesdef)
                    ranking<-c(1:aantalrij)
                    o<-order(rijmortrates,hospitals)
                    lijstdef<-cbind(ranking,hospitals[o],rijmortrates[o])}
if(rank=="best") {rank<-1} else
    if(rank=="worst") {rank<-aantalrij} else
        if(rank>aantalrij) {print(NA)}
print(lijstdef[rank,])
}







    
    
    
    
    
    
    
    
    
    
    
    dha<-as.numeric(dfstate[,11])
    print(sort(dfstate[as.numeric(dfstate[,11])==dhamin,2]))} else
        if(outcome == "heart failure"){
            dhfmin<-min(as.numeric(dfstate[,17]),na.rm = TRUE)
            print(sort(dfstate[as.numeric(dfstate[,17])==dhfmin,2]))} else
                if(outcome == "pneumonia"){
                    dpnmin<-min(as.numeric(dfstate[,23]),na.rm = TRUE)
                    print(sort(dfstate[as.numeric(dfstate[,23])==dpnmin,2]))}
}

}