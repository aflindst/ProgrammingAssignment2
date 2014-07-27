## Programming assignment 2
## Adam Lindstrom
## These function construct and modify a variant of the matrix object which is able to cache its own matrix inverse.
## Use them for large matrices with long solve() times.

## makeCacheMatrix() takes a matrix as its only argument and returns a CacheMatrix object
## the CachMatrix object has two locally defined variables:
##    x, the matrix
##    i, the cached inverse of x
## the CacheMatrix object is a list of 4 functions (methods):
##    get() returns the value of x
##    set(y) assigns the new value y to x, then reinitializes i
##    getinv() returns the value of i
##    set(inv) assigns a new value inv to i
## Example:
## test<-makeCacheMatrix(matrix(1:4, c(2,2)))
## test$get()
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve(CacheMatrix) returns the inverse of a CacheMatrix.
## cacheSolve() uses the getinv() and setinv() methods of a CacheMatrix to:
##    1. check if the CachMatrix has a valid value from getinv(). If so, return the cached value.
##    2. If not, solve(), use setinv to cache the result of solve(), then return it.
## Example:
##    test<-makeCacheMatrix(matrix(1:4, c(2,2)))
##    cacheSolve(test)
##    cacheSolve(test)
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  message("computing inverse for the first time")
  i <- solve(data, ...)
  x$setinv(i)
  i
}
