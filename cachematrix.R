## The following functions are used to create a special matrix object and cache it's inverse


## This function creates a special object than can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

## This function finds the inverse of matrix either through the stored value or computing it

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
    if (!is.null(inv)) {
       return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

