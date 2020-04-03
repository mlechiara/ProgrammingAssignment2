## The following are a pair of functions that calculate and cache the inverse of a given
## matrix. Matrix computation, usually being a costly computation can benefit from storing
## values that can be returned at a later time.

## The makeCacheMatrix function creates a list the contains a function
## that sets the matrix values of the vector, gets the matrix values of the vector,
## sets the values of the inverse of a matrix, and hets the values of the inverse
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function calculates the inverse returned by the makeCacheMatrix
## function. First it checks to see if the inverse has already been calculated, and
## if so returns the cached inverse.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
}
