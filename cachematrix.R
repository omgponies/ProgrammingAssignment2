# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse 
# of a matrix rather than computing it repeatedly.
# These two functions do that.

# makeCacheMatrix creates a list containing a function to
# * set the value of the matrix
# * get the value of the matrix
# * set the value of inverse of the matrix
# * get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                inv <<- NULL
                x <<- y                
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function returns the inverse of the matrix. 
# * check if inverse has already been computed
#       * if yes,  get result and return
#       * if no, compute the inverse, set the value in the cache 
#               (using setinverse function)
# This assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
