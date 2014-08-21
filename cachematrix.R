## This code demonstrates how to write a special caching
## matrix that can store the result of a matrix inversion
## calculation such that the result object can be
## re-used if the matrix hasn't changed. 

## Creates a special matrix that can cache its inverse, 
## taking advantage of R's lexical scoping rules

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of a matrix, returning the 
## cached result object, if it exists. Otherwise, it
## computes the inverse of the matrix and caches it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
