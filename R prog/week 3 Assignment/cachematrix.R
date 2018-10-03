

makeCacheMatrix <- function(x = matrix()) {
    i<-matrix(NULL)
    set <- function(y) {
        x<<-y
        i<<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) i<<-inverse   ## set inverse value to cache
    getinverse<- function() i
    list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
    i<- x$getinverse()   ## if inverse already exists
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data, ...)
    x$setinverse(i)
    
    i    ## Return a matrix that is the inverse of 'x'
}
