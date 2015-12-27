## These functions 1) cache a matrix and 2) pull the marix cached in the first function, 
## and, if the inverse has not yet been calcuated, calcuate the inverse of the matrix 

## This function caches a version of a special of a matrix, so that it's inverse can
## be calcuated in the next function. The if the inverse has already been calcualted,
## function will cache the inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function pulls the cached matrix from the above function.
## If the inverse has already been calculated, it will return the cached function;
## if not, it will take the inverse and return that.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

