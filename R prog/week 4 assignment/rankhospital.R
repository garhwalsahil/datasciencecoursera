rankhospital<-function(state,outcome,rank){
    
    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    data1<-as.data.frame(cbind(data[,2], 
                               data[,7],
                               data[,11],
                               data[,17],
                               data[,23]),
                         stringsAsFactors = FALSE)
    colnames(data1)<-c("hospital", "Hstate", "heart attack", "heart failure",
                       "pneumonia")
    if(!state %in% data1[,"Hstate"]){
        stop("invalid state")
    }
    else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid state")
    }
    
    
    data2<-cbind(data1[,"hospital"]
                 [which(data1[,"Hstate"]==state)]
                 ,suppressWarnings(
                     as.numeric(data1[,outcome]
                                              [which
                                                  (data1[,"Hstate"]==state)
                                                  ]
                                )
                     )
                 )
    colnames(data2)<-c("hospital",outcome)
    
    data3<-data2[order(data2[,outcome],data2[,"hospital"])]
    
    data4<-data2[order(data2[,outcome],data2[,"hospital"],
                       decreasing = TRUE)]
    
    if(is.numeric(rank)==TRUE){
        return(data2[rank])
    }
    else if(rank=="best"){
        return(data3[1])
    }
    else if(rank=="worst"){
        return(data4[1])
    }
    else {stop("invalid rank")}
    
    
    
    
    
    
}

