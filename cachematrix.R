# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
# This pair of functions will cache the inverse of a matrix.

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
} 



## This function calculates the inverse of the "matrix" created with the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
      i <- x$getInverse()
      if(!is.null(i)){
            message("Getting cached data.")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setInverse(i)
      i
}