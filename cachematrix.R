## This file contains two functions that provide a simple way to optimize inverse matrix computation; they are
## makeCacheMatrix  - returns a decorated matrix that can store its cached inverse
## cacheSolve - returns the inverse of a matrix, leveraging its cached if available

## This function returns an object that implements the Decorator pattern. This object wraps a
## matrix and provides storage for the matrix's inverse so that a cached version of the inverse
## can be used in subsequent computations.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                # make sure to clear out the cached inverse
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv

        # Return a list with all necessary functions to get/set both the original matrix, and its
        # inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function takes the object returned by makeCacheMatrix, and returns the inverse of the
## matrix that it wraps. If the inverse matrix has not already been set with setinverse function, then
## the cached inverse is returned. If not, the inverse is computed and then set on the object
## passed in.

cacheSolve <- function(x, ...) {
       ## attempt to get inverse from cache
       inv <- x$getinverse()
        if(!is.null(inv)) {
                return(inv)
        }

        # inverse was not in cache, so now we have to calculate it
        m <- x$get()

        # solve actually does much more than  compute the inverse of a matrix, but since that is
        # all we want it to do, we'll leave the second argument blank.
        inv <- solve(m, ...)

        # add inverse to the cache so that future calls will be faster
        x$setinverse(inv)
        inv 
}

