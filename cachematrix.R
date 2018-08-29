# Creates a matrix that can cache it's inverse
#
# Args:
#   x: A matrix
# Return:
#   A list with functions to get/set value and get/set inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function(solve) s
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


# Computes the inverse of a matrix. The cached inverse is returned, if the inverse has already been
# calculated before and matrix hasn't changed
#
# Args:
#   x: A matrix
#   ...: Extra arguments
# Return:
#   The inverse of the matrix

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

# Test and examples 
m <- matrix(c(2, 4, -3, -6), nrow = 2, ncol = 2)
mc <- makeCacheMatrix(m)
cacheSolve(mc)


