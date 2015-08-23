##These functions, together, create a matrix with which to store the inverse of a matrix, and then either finds the inverse itself, or locates the cached inverse matrix.

## This function creates a matrix with which to store the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function looks for a cached inverse, or calculates the inverse if it has not already been cached.


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
