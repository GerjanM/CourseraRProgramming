rankall<-function(outcome,rank="best"){
    ## Read outcome data
    df<-read.csv("D:/Users/Gerjan/R/ProgramAssignment3data/outcome-of-care-measures.csv",colClasses="character")
    ##check op outcome
    if(rank=="best") {rank<-1}
    trueoutcome<-c("heart attack","heart failure","pneumonia")
    g<-trueoutcome[]==outcome
    h<-trueoutcome[g]
    if(length(h)==1) print(" ") else stop("invalid outcome", call. = TRUE)
    uniqueState<-sort(unique(df[,7]))
    
    ## For each state, find the hospital of the given rank
for (i in uniqueState) {
    lijst<-df[df[7]==i,c(2,7,11,17,23)]
    rankhosp<<-data.frame()
        if(outcome=="heart failure"){
        hospitals<-lijst$Hospital.Name
        rijmortrates<-as.numeric(lijst$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        aantalrij<-nrow(lijst)
        state<-rep(i,aantalrij)
            if(rank=="worst") {
            rank<-aantalrij
            ranking<-c(1:aantalrij)
            o<-order(rijmortrates,hospitals,state)
            lijstdef<-data.frame(state,hospitals[o])
            rankhosp<-rbind.data.frame(rankhosp,lijstdef[rank,])} 
            else if(aantalrij<rank) {
            NArank<-data.frame(i,"<NA>")
            names(NArank)<-c("state","hospitals.o.")
            rankhosp<-rbind.data.frame(rankhosp,NArank)} 
            else{
            ranking<-c(1:aantalrij)
            o<-order(rijmortrates,hospitals,state)
            lijstdef<-data.frame(state,hospitals[o])
            rankhosp<-rbind.data.frame(rankhosp,lijstdef[rank,])}}
        else if(outcome=="heart attack"){
        hospitals<-lijst$Hospital.Name
        rijmortrates<-as.numeric(lijst$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        aantalrij<-nrow(lijst)
        #print(aantalrij)
        state<-rep(i,aantalrij)
            if(rank=="worst") {
            ranking<-c(1:aantalrij)
            rank<-aantalrij
            o<-order(rijmortrates,hospitals,state)
            lijstdef<-data.frame(state,hospitals[o])
            rankhosp<-rbind.data.frame(rankhosp,lijstdef[rank,])} 
            else if(aantalrij<rank) {
            NArank<-data.frame(i,"<NA>")
            names(NArank)<-c("state","hospitals.o.")
            rankhosp<-rbind.data.frame(rankhosp,NArank)} 
            else{
            ranking<-c(1:aantalrij)
            o<-order(rijmortrates,hospitals,state)
            lijstdef<-data.frame(state,hospitals[o])
            rankhosp<-rbind.data.frame(rankhosp,lijstdef[rank,])}}
        else if(outcome=="pneumonia"){
        hospitals<-lijst$Hospital.Name
        rijmortrates<-as.numeric(lijst$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        aantalrij<-nrow(lijst)
        state<-rep(i,aantalrij)
            if(rank=="worst") {
            rank<-aantalrij
            ranking<-c(1:aantalrij)
            o<-order(rijmortrates,hospitals,state)
            lijstdef<-data.frame(state,hospitals[o])
            rankhosp<-rbind.data.frame(rankhosp,lijstdef[rank,])} 
            else if(aantalrij<rank) {
            NArank<-data.frame(i,"<NA>")
            names(NArank)<-c("state","hospitals.o.")
            rankhosp<-rbind.data.frame(rankhosp,NArank)} 
            else{
            ranking<-c(1:aantalrij)
            o<-order(rijmortrates,hospitals,state)
            lijstdef<-data.frame(state,hospitals[o])
            rankhosp<-rbind.data.frame(rankhosp,lijstdef[rank,])}}
}
        ## Return a data frame with the hospital names and the (abbreviated) state name
            print(rankhosp)
        }           
            
            
           
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
        
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


