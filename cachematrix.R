## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates an object that can receive a matrix and 
## store a cache of the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv_in) inv <<- inv_in
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
# Solves the inverse of the input matrix and stores in cache.
# On a second call - the cached inverse will be used.
# returns the inversed matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
