
## The first function, `makeCacheMatrix` creates a "matrix", which is really a list containing a function that
## sets the value of the matrix, gets the value of the matrix, sets the value of the inverse, and lastly, gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the "matrix" created with the above function. First, the function checks to see if the inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. If the inverse has not been calcuted, then the function calculates the inverse of the data and sets the value of the inverse in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
}
