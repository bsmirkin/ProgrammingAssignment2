## Programming Assignment 2
## January 16, 2026
## Brian Mirkin

## makeCacheMatrix
## Creates an object containing a matrix with a cache of its inverse.
## This object includes set and get functions

makeCacheMatrix <- function(x = matrix()) {
  #inv will contain the inverse of the matrix, when computed
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list (set=set,get=get,setinv=setinv,getinv=getinv)
}


## Requires special matrix object created by makeCacheMatrix
## Ensures that the inverse is in the object
## If a cached value is present, do not recompute the inverse

cacheSolve <- function(x, ...) {
 
  i <- x$getinv()
  # if the inverse is not in the object x, compute and add it
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # inverse is not cached. Compute and cache it.
  
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}
