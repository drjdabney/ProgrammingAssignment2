## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function () x
    setInverse <- function (inv) inverse <<- inv
    getInverse <- function () inverse
    list (set = set, get = get, setInverse = setInverse, 
          getInverse = getInverse)
}


## cacheSolve: computes inverse of "matrix" returned by makeCacheMatrix;
##             retrieves inverse from cache if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
