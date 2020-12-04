## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Defines the get/set functions
## Defines the matrix, x, and the inverse, s
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) { ##defines the set function
      x <<- y
      s <<- NULL ##assigns values in parent scopes
    }
    get <- function() x ##defines the get function
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s ##define in parent scope and get/set/Solve functions
    list(set = set,
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}
## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## 'Solve' function without additional arguments will return inverse matrix
cacheSolve <- function(x, ...) {
  s <- x$getSolve() ##retrieve matrix 's'
  if (!is.null(s)) {
    message("getting cached data") ##lets one know if the matrix has been done before
    return(s)
  }
  data <- x$get() ##retrieve matrix 'x'
  s <- solve(data, ...)
  x$setSolve(s) ##set matrix 's'
  s
}