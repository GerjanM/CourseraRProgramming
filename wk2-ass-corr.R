##---------deze is goed en gecommit in GIT !!! ---------

corr<-function(directory,treshold=0){
    filelist<-list.files(path=directory,pattern="*.csv",full.names=TRUE)
    output<-numeric()
    #print(filelist)
    for(i in 1:332){
        data<-read.csv(filelist[i])
        #print(data)
        good<-complete.cases(data)
        #print(good)
        numrows<-nrow(data[good,])
        if (numrows > treshold){
        goodrow2<-data[good,2]
        goodrow3<-data[good,3]
        output2<-cor(goodrow2,goodrow3)
        output<-c(output,output2)
        }
    }
    print(output)
}

corr("D:/Users/Gerjan/Documents/R/specdata",150)
