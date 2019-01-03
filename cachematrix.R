## The functions below are utilized to store and cache the inverse of a matrix

## This function creates a special "matrix" object with the capability of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
            inverse <- NULL
            set <- function(y) {
                    x <<- y
                    inverse <<- NULL
            }
            get <- function() x
            setinverse <- function(solveMatrix) inverse <<- solveMatrix
            getinverse <- function() inverse
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## This function caches the inverse of the matrix created above, if already calculated. If not, it computes for the inverse.

cacheSolve <- function(x, ...) {
          inverse <- x$getinverse()
          if(!is.null(inverse)) {
                      message("retrieving cached data")
                      return(inverse)
          }
          data <- x$get()
          inverse <- solve(data)
          x$setinverse(inverse)
          inverse
}
        ## Return a matrix that is the inverse of 'x'

