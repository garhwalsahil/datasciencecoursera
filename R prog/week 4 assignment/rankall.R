rankall<-function(outcome,num){
    
    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    data1<-as.data.frame(cbind(data[,2], 
                               data[,7],
                               data[,11],
                               data[,17],
                               data[,23]),
                         stringsAsFactors = FALSE)
    colnames(data1)<-c("hospital", "Hstate", "heart attack", "heart failure",
                       "pneumonia")
    
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid state")
    }
    ordered<-list()
    data2<-with(data1,split(data1,Hstate))
    if(is.numeric(num)){
        for(i in seq_along(data2)){
            data2[[i]]<-data2[[i]][order
                                   (suppressWarnings(
                                       as.numeric(data2[[i]][,outcome])),
                                       data2[[i]][,"hospital"]),]
            ordered[[i]]<-c(data2[[i]][num,"hospital"],data2[[i]][1,"Hstate"])
        
        
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = result[, 2], 
                                stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
    }
    if(num=="best"){
        for(i in seq_along(data2)){
            data2[[i]]<-data2[[i]][order
                                   (suppressWarnings(
                                       as.numeric(data2[[i]][,outcome])),
                                       data2[[i]][,"hospital"]),]
            ordered[[i]]<-c(data2[[i]][1,"hospital"],data2[[i]][1,"Hstate"])
            
            
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = result[, 2]
                                , stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
        
    }
    if(num=="worst"){
        for(i in seq_along(data2)){
            data2[[i]]<-data2[[i]][order
                                   (suppressWarnings(
                                       as.numeric(data2[[i]][,outcome])),
                                       data2[[i]][,"hospital"],
                                       decreasing = TRUE),]
            ordered[[i]]<-c(data2[[i]][1,"hospital"],data2[[i]][1,"Hstate"])
            
            
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = result[, 2], 
                                stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
        
    }
                              
    
    output
    
}

