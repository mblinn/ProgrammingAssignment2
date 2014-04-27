## Functions to make a cache matrix and compute its inverse.

# Returns a cache matrix
# A cache matrix is a list of four functions, set, get, setinv and getinv
# set/get set a matrix, setinv/getinv store get and set its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invToSet) inv <<- invToSet
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Compute the inverse of a cache matrix.  
# If the inverse of the cache matrix has already been computed, returns the cached value, otherwise
# the inverse is computed and stored in the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("using cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
