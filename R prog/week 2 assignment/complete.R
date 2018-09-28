complete<-function(path ,id=1:332){
    path1 <- paste0(getwd(),"/", path) 
    
    data <- data.frame()
    data1<-data.frame()
    for (i in id) {
        if (i < 10) {
            dat <- read.csv(paste(path1,"/00", as.character(i),".csv", sep = ""))
            #data <- rbind(data,dat)
            #good<-complete.cases(data)
            #data1<-data[good,][,]
        }
        else if (i < 100) {
            dat <- read.csv(paste(path1,"/0", as.character(i),".csv", sep = ""))
            #data <- rbind(data,dat)
            #good<-complete.cases(data)
            #data1<-data[good,][,]
        }
        else {
            dat <- read.csv(paste(path1,"/", as.character(i),".csv", sep = ""))
            #data <- rbind(data,dat)
            #good<-complete.cases(data)
            #data1<-data[good,][,]
        }
        data2<-data.frame(i,nobs = sum(complete.cases(dat)))
        data<-rbind(data,data2)
    }
   
    return(data)
}

