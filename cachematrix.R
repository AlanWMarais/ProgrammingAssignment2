## These functions accept a square invertible matrix and return it's inverse by using the
## solve() function.
## For efficiency reasons the inverted matrix is derived and then cached.
## If the function is called again with the same matrix then the inverted matrix is retrieved
## from cache. If it is a new matrix then the inverted matrix is calculated using the
## solve() function.

## This function creates a special matrix object that can have its inverse cached

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) s <<- solve
     getsolve <- function() s
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## This function computes the inverse of the special square matrix returned 
## by makeCacheMatrix. If the inverse has already been calculated and the square matrix
## hasnt changed then the inverse is retrieved from cache rather than being recalculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
