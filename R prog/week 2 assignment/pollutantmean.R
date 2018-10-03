pollutantMean<-function(path , pollutant , id=1:332){
    path1 <- paste0(getwd(),"/", path) 
    
    data <- data.frame()
    data1<-data.frame()
    for (i in id) {
        if (i < 10) {
            dat <- read.csv(paste(path1,"/00", as.character(i),".csv", sep = ""))  #reading files from 001.csv to 009.csv
            data <- rbind(data,dat)
            good<-complete.cases(data)
            data1<-data[good,][,]      #removing na values
        }
        else if (i < 100) {
            dat <- read.csv(paste(path1,"/0", as.character(i),".csv", sep = ""))  #reading files from 010.csv to 099.csv
            data <- rbind(data,dat)
            good<-complete.cases(data)
            data1<-data[good,][,]      #removing na values
        }
        else {
            dat <- read.csv(paste(path1,"/", as.character(i),".csv", sep = ""))  #reading files from 100.csv to 332.csv
            data <- rbind(data,dat)
            good<-complete.cases(data)
            data1<-data[good,][,]     #removing na values
        }
        
    }
    mean(data1[,pollutant])
    

    #good<- complete.cases(data)
    #data2<-data[good,][,]    
    #mean(data2[[pollutant]])
    
}

