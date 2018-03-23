##makeCacheMatrix and cacheSolve give the inverse of a matrix.
##The inverse is computed and cached if not already cached.

## Returns a list of functions.
## Get and set the matrix.
## Get and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) i <<- inv
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## Returns the inverse of the matrix.
## If inverse is cached, returns cached value directly.
## Computes inverse otherwise. 
## Returns NULL if matrix is not square.

cacheSolve <- function(x, ...) {
  
  if(ncol(x$get()) != nrow(x$get())) {
    message("Not square matrix.")
    return (NULL)
  }
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
  
  
}

