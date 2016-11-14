
## This is a programming assignment. The solution follows a specified template
## and it is not intended to be production code.

## These are helper functions relating to calculating the inverse of a matrix
## and caching the result.


## Wraps a specified matrix in a cache object. This cache is intended to be used
## with the 'cacheSolve' function. Comprises a list with labels:
##      set: Function which sets the matrix to be inverted.
##      get: Function which gets the matrix to be inverted.
##      setinverse: Sets the cached inverse.
##      getinverse: Gets the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { x <<- y; inv <<- NULL }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Based on a specified cache object (as created by the 'makeCacheMatrix'
## function), returns the inverse of the wrapped matrix by either computing the
## value or by retrieving a cached value if this is available. The cache is
## updated if a value is computed.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
