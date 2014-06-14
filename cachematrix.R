## Functions for creating a matrix, calculating its inverse matrix and caching it
## to avoid recalculation

## Wrap a matrix inside a list, providing function to get/set the matrix,
## and get/set the invert of the matrix. The matrix is assumed to be invertable
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        list(
             get = function() x,
             getinv = function() inv,
             setinv = function(invert) inv <<- invert,
             set = function(y) {
                     x <<- y
                     inv <<- NULL
             }
        )

}


## Return the invert of the given matrix. The result is cached to
## avoid recalculating it the next time it's requested. The input matrix is
## expected to have been created by makeCacheMatrix
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve( x$get(), ... )
        x$setinv( inv )
        inv
}
