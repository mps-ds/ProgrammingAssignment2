## The makeCacheMatrix and cacheSolve functions together make
## it possible to avoid expensive matrix inversion operations
## on the same matrix by caching the result.

## makeCacheMatrix creates an object for storing
## a matrix and its inverse. The inverse is calculated
## by calling the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list (set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## cacheSolve takes in a CacheMatrix object and
## retrieves its inverse from the cache if possible
## and calculates and caches it otherwise.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
