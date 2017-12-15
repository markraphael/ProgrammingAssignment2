## Creates a matrix object which caches its inverse

## Creates the matrix object described above

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getInverse <- function() inv
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  list(get = get, set = set, 
       getInverse = getInverse, 
       setInverse = setInverse)
}


## Retrieves the cached inverse of the matrix object passed in, 
## or calculates the inverse if it has not been cached yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) return(inv)
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  inv
}
