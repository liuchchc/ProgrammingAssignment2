## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## store the matrix inverse value
    invValue <- NULL
    ## set the matrix value
    set <- function(y) {
        x <<- y
        invValue <<- NULL
    }
    ## get the matrix value
    get <- function() x
    ## set the matrix inverse value
    setInverse <- function(solve) invValue <<- solve
    ## get the matrix inverse value
    getInverse <- function() invValue
    
    ## return the datastructure whith contains servral opt
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    ## get the cached matrix inverse value
    invValue <- x$getInverse()
    ## if the matrix inverse value is not null,return
    if(!is.null(invValue)) {
        message("getting cached data")
        return(invValue)
    }
    
    ## check the matrix's row number and column number
    data <- x$get()
    if(nrow(data) != ncol(data)){
        message("data must be square")
        return(NULL)
    }
    
    ## cacluate the matrix inverse value
    invValue <- solve(data, ...)
    ## cache the matrix inverse value
    x$setInverse(invValue)
    ## return the matrix inverse value
    invValue
}
