## Functions to cache the inverse of a square matrix so it can be re-used 
## when needed.  When the inverse is read from the cache "getting cached data"
## messages is displayed.
##
## Sample usage:
## > x <- matrix(c(1,2,3,4), nrow=2, ncol = 2)  # create 2x2 matrix
## > x1 <- makeCacheMatrix(x)                   # create a "special" matrix
## > cacheSolve(x1)                             # calculate inverse matrix
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## > cacheSolve(x1)                             # get inverse from cache
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##> 
##
## Function which creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) inverse <<- solve
    
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated and the matrix
## has not changed, then the cachesolve retrieves the inverse from the cache.
## 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    #print("inverse:")
    #print(inverse)
    
    ## If inverse is not NULL we have inverse matrix and can return
    ## the results imediately.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## No inverse cached yet. We compute it and store for
    ## the future use.
    data <- x$get()
    #print("data:")
    #print(data)
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
