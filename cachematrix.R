## Functions for caching the inverse of a square matrix, to reduce unnecessary
## computation

## Returns a list of functions that can get or set a matrix, and get or set
## (aka, cache) that matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'. If the inverse of 'x' has
## already been computed and cached, return that instead of doing the
## computation.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
