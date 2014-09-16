# Assignment: Caching the Inverse of a Matrix
# 
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
# These functions create a "matrix" object containing a matrix and a cached 
# copy of its inverse. If this inverse needs to be computed again the cached copy 
# can be accessed

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# assumes x is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x 
  setInv <- function(solve) I <<- solve
  getInv <- function() I
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInv(I)
  I
}
