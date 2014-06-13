## ---------------------------------------------------------------------
## Cached Matrix Inversion
## ---------------------------------------------------------------------
##
## Set of functions that implement a mecanism for caching the result
## of inverting a numeric matrix. The set has two functions :
##
## - makeCacheMatrix : stores the matrix and the inverted matrix, when
##                     already calculated.
## - cacheSolve      : returns the inverted matrix, either using the solve
##                     function or the cached resulted from a previous
##                     calculation.

## ---------------------------------------------------------------------
## makeCacheMatrix
##
## Function that returns a list of 4 functions that allows to store and
## access a matrix and it's inverse matrix. The function received as a
## parameter the inicial matrix. Documentation for each of this function 
## inline in the function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # set : assign a new matrix and invalidate the cached inversion
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get : return the associated matrix
  get <- function() x
  
  # setinv : assign a new inverted matrix to the cache
  setinv <- function(inv) i <<- inv
  
  # getinv : return the inversion of the associated matrix
  getinv <- function() i
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## ---------------------------------------------------------------------
## cacheSolve
##
## Function that returns the inverted matrix of a matrix stored in an
## object returned by makeCacheMatrix. This inverted matrix can be read
## from cache (if calculated before) or calculated on the fly and stored
## in cache for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
