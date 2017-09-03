## These functions work together to cache the inverse of a matrix.  This will save time in computation when the
## same inverse matrix value is needed in more than one instance.  These functions will allow the value to be 
## called from memory rather than recomputed every time it is needed.

## This function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of a matrix.  As its argument, it takes a special "matrix" object as defined in
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
