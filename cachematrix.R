## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.


## Creating a matrix object than can cache its inverse. This function:
## set the value of the matirx
## get the value of the matirx
## set the inverse of the matirx
## get the inverse of the matirx

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
        set <- function(y) {
               	x <<- y
               	inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of the matirx created with the makeCacheMatrix function, if the inverse has not already been calculated.
## If it has been calculated before, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
