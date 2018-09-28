corr<-function(path ,threshold=0){
    path1 <- paste0(getwd(),"/", path) 
    
    data <- data.frame()
    data1<-data.frame()
    corr_vect<-NULL
    for (i in 1:332) {
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
        data1<-dat[complete.cases(dat),]
        if(nrow(data1)>threshold){
            corr_vect<-c(corr_vect,cor(data1[,"sulfate"],data1[,"nitrate"]))
        }
    }
    
    return(corr_vect)
}

