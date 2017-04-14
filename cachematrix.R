## makeCacheMatrix and cacheSolve together solve, store and retrieve
## the inverse of invertable nxn matrices

## makeCacheMatrix sets up the functions to solve and store an inverse
makeCacheMatrix <- function(x = matrix) { ## assumes invertible input
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}

## cacheSolve takes as input the result of makeCacheMatrix stored
## in a varible and returns the cached inverse
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
