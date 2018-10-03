##make cache matrix takes matrix as input and returns a special matrix with 
##cache information about it's inverse and implements list of functions on it


makeCacheMatrix <- function(x = matrix()) {
    i<-matrix(NULL)
    set <- function(y) {
        x<<-y
        i<<-NULL
    }
    get <- function() x            ##gets the matrix
    setinverse <- function(inverse) i<<-inverse   ## set inverse value to cache
    getinverse<- function() i    # gets the inverse matrix
    list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
    i<- x$getinverse()   ## gets cached value of inverse matrix
    if(!is.null(i)){     ## if cache not null inverse matrix value exists
        message("getting cached data") 
        return(i)       ## returns that value
    }
    data<-x$get()         ## if inverse matrix doesn't exists calculates it
    i<-solve(data, ...)
    x$setinverse(i)           ## sets the calculated value tovalue of inverse matrix 
    
    i    ## Return a matrix that is the inverse of 'x'
}
