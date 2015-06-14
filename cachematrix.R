## These two functions serve to reduce the time spent (usually within a loop)
## on computing the inverse of a matrix that has already been done before. 
## It caches the result and allows it to be called upon, if it is not changing, 
## instead of reperforming the solve() function.

## This function creates a special "matrix" object that can cache its inverse.
## It defines and contains a list of functions that can be individually called
## upon to either set objects or get their values.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(matrix) m <<- matrix
  getinv <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache. This function will only work if the matrix
## is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
  
}
