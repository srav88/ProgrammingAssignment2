## Programming Assignment 2 ##
## R Programming ##
## Code wrtten by Sravan Rajupalem ##

## This code caches the inverse of a matrix.
## Since matrix inversion can be a time consuming computation,
## if the contents of the matrix are not changing it is useful to cache
## the value of the inverse instead of recomputing it.

## This code creates contains two functions that store a matrix and caches the inverse.


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## END OF CODE ##
