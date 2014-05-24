## Put comments here that give an overall description of what your
## functions do

## Takes a matrix as input, and outputs a list which has four functions as its 
## elements: set, get, setinv and getinv. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Look at the list created by makeCacheMatrix(), and if it hasn't already been 
## cached, calculates the inverse of the matrix which is 'set'; and cache it. 
## If it has already been cached, print 'getting cached data' and then retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
