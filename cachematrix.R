## To save time in calculating the inverse of a matrix to be used across multiple
## computations, the following functions create a cache of the inverted matrix
## that allows it to be easily recalled and used in future computations.



## This first function creates a special "matrix" object that can cache its inverse.

##makeCacheMatrix creates a special "matrix", which is really a list containing
##a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function () m
  list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This next function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
## matrix has not changed), then `cacheSolve` should retrieve the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the mean in the cache via the setmatrix
## function.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
