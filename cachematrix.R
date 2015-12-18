## These functions calculates the inverse of a matrix and stores 
## the result in cache memory. If the inverse is going to be 
## assessed again, then they recover the inverse with no need of
## evaluating the inverse again. 

## This function inverts the matrix and save the 
## results in cache memory (cache its inverse).
## x is matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function searches a matrix stored in cache 
## memory (m). If there is a matrix stored which 
## matches x, then it returns inverse matrix in cache 
## memory either way it computes the inverse using
## solve and returns the inverse matrix.
## x is makeCacheMatrix class matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}