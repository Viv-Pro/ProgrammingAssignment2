## These functions work together to create a special "matrix" object that can 
## cache (store) its inverse. 

## makeCacheMatrix creates a special "matrix" object that can store its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse is already cached, it retrieves the 
## stored result instead of recomputing it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

