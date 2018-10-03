complete<-function(path ,id=1:332){
    path1 <- paste0(getwd(),"/", path) 
    
    data <- data.frame()
    data1<-data.frame()
    for (i in id) {
        if (i < 10) {
            dat <- read.csv(paste(path1,"/00", as.character(i),".csv", sep = "")) #reading files from 001.csv to 009.csv
            #data <- rbind(data,dat)
            #good<-complete.cases(data)
            #data1<-data[good,][,]
        }
        else if (i < 100) {
            dat <- read.csv(paste(path1,"/0", as.character(i),".csv", sep = "")) #reading files from 010.csv to 099.csv
            #data <- rbind(data,dat)
            #good<-complete.cases(data)
            #data1<-data[good,][,]
        }
        else {
            dat <- read.csv(paste(path1,"/", as.character(i),".csv", sep = ""))  #reading files from 100.csv to 332.csv
            #data <- rbind(data,dat)
            #good<-complete.cases(data)
            #data1<-data[good,][,]
        }
        data2<-data.frame(i,nobs = sum(complete.cases(dat))) #counting complete cases and adding it to data2 with i
        data<-rbind(data,data2)
    }
   
    return(data)
}

