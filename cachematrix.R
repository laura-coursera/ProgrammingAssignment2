## A set of functions to
## - make a matrix where the inverse can be cached
## - take such a matrix and compute it's inverse, which may or may not be cached.

## makes a cache-able matrix with get, set, setInverse and getInverse functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    # gets the data from the matrix
    get <- function() {
        x
    }
    
    # sets the matrix x to be a new matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # sets the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    # gets the cached inverse of the matrix
    # will return NULL if it has not been set
    getInverse <- function() {
        i
    }
    
    # return list of functions
    list(get=get, set=set, setInverse = setInverse, getInverse=getInverse)
}


## solves a cache-able matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    # try to get cached inverse
    i <- x$getInverse()
    
    if (!is.null(i)) {
        message("getting cached data")
    } else {
        # if 'i' is null, we must solve the matrix stored at x$get()
        # set that to i, and set the inverse for future use
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
    }
    
    # return the inverse i
    i
}

