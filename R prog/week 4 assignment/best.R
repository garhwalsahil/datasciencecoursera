best<-function(state,outcome){
    
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
    else{
       
       min_val<- tapply(suppressWarnings(as.numeric(data1[,outcome])),
                        data1[,"Hstate"],function(x) 
                        min(x,na.rm = TRUE),
                        simplify = FALSE)
       
       result<-data1[,"hospital"][which(
           suppressWarnings( 
               as.numeric(data1[,outcome]))==min_val[state] &
               (data1[,"Hstate"]==state))]
       
       output<-result[order(result)]
        
    }
    output
         
}