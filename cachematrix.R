## This programming assignment requires writing an R function which is able to cache
## potentially time-consuming computations ... in this case the inverse of a matrix.
##
## eak ... 102215

## The first function makeCacheMatrix creates a special "matrix" object that can cache  
## its inverse. It is really a list containing functions to:
## 
## set ... set the matrix
## get ... get the matrix
## setInverse ... set (cache) the inverse of the matrix
## getInverse ... get the inverse of the matrix
##
## Make the matrix ... m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
## View the matrix ... m$get()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {    
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.  If the inverse has already been calculated (and the 
## matrix has not changed), then cachesolve will retrieve the inverse from the cache.
##
## Solve/View the inverse ... cacheSolve(m)

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data ...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
