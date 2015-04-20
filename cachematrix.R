## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invValue <- NULL
    set <- function(y) {
        x <<- y
        invValue <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) invValue <<- solve
    getInverse <- function() invValue
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invValue <- x$getInverse()
    if(!is.null(invValue)) {
        message("getting cached data")
        return(invValue)
    }
    
    data <- x$get()
    if(nrow(data) != ncol(data)){
        message("data must be square")
        return(NULL)
    }
    
    invValue <- solve(data, ...)
    x$setInverse(invValue)
    invValue
}
