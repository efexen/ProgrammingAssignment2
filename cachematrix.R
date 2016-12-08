## Functions to invert a matrix and cache the results

## A special type of matrix that can hold a cache of
## its inverse

makeCacheMatrix <- function(matrix = matrix()) {
  cache <- NULL
  set <- function(newMatrix) {
    matrix <<- newMatrix
    cache <<- NULL
  }
  get <- function() { matrix }
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function() { cache }
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## Function to inverse a matrix and store the value
## in a cache or retrieve cached value if one exists

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- cacheMatrix$get()
  inverse <- solve(matrix, ...)
  cacheMatrix$setInverse(inverse)
  inverse
}
