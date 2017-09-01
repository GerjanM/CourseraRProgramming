
## in GIT ##
## deze werkt! ##
pollutantmean<-function(pollutant,id=1:332){
    fn<-list.files("D:/Users/Gerjan/Documents/R/specdata",pattern="*.csv",full.names=TRUE)
    values<-numeric()
    for(i in id){
        data<-read.csv(fn[i])
        values<-c(values,data[[pollutant]])
    }
    mean(values,na.rm=TRUE)
}

pollutantmean("sulfate")

## deze werkt ook! ##

pollutantmean<-function(directory, pollutant,id=1:332){
    fn<-list.files(path=directory,pattern="*.csv",full.names=TRUE)
    values<-numeric()
    for(i in id){
        data<-read.csv(fn[i])
        values<-c(values,data[[pollutant]])
    }
    mean(values,na.rm=TRUE)
}

pollutantmean("D:/Users/Gerjan/Documents/R/specdata","sulfate")



optellen<-function(x,y){
    a<-x+y
    a
}
z<-a

nums<-c(2,6,3)

complete<-function(directory,id=1:322){
    filelist<-list.files(path=directory,pattern="*.csv",full.names=TRUE)
    value<-numeric()
    for(i in id){
        data<-read.csv(filelist[i])
        output<-numeric()
        outputnew<-numeric()
    }
        good<-complete.cases(data)
        goodrow<-data[good, ]
        ##print(goodrow)
        numrows<-nrow(goodrow)
        print(numrows)
        outputnew<-c(numrows,2)
        print(outputnew)
}


complete<-function(directory,id=1:322){
    filelist<-list.files(path=directory,pattern="*.csv",full.names=TRUE)
    outputdf<-numeric()
    for(i in id){
        data<-read.csv(filelist[i])
        good<-complete.cases(data)
        goodrow<-data[good, ]
        numrows<-nrow(goodrow)
        outputnew<-c(i,numrows)
        outputdf<-rbind(outputdf,outputnew)
        colnames(outputdf)<-c("id","nobs")
    }
    print(outputdf)
}   
        
complete("D:/Users/Gerjan/Documents/R/specdata",30:25)      


complete<-function(directory,id=1:322){
    filelist<-list.files(path=directory,pattern="*.csv",full.names=TRUE)
    outputdf<-numeric()
    for(i in id){
        data<-read.csv(filelist[i])
        good<-complete.cases(data)
        goodrow<-data[good, ]
        numrows<-nrow(goodrow)
        outputnew<-c(i,numrows)
        outputdf<-rbind(outputdf,outputnew)
        colnames(outputdf)<-c("id","nobs")
    }
    print(outputdf)
}   
