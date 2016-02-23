## Functions that compute and cache the inverse of a matrix
## to avoid repeated inverse calculations for the same matrix.

## Creates a special type of a matrix object that supports
## inversion caching.
## The input matrix must be invertible.

## Example:
## x <- matrix(c(7, 0, -3, 2, 3, 4, 1, -1, -2), 3, 3)
## m <- makeCacheMatrix(x)
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Calculate and cache the inverse of the matrix once.
## Then return the cached inverse value.
## The input matrix must be created using the makeCacheMatrix function.

## Example:
## cacheSolve(m)
## cacheSolve(m)
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
