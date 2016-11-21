## The first function creates a list of functions that get and set the value of the input
## and get and set the inverse of the matrix input.
## The second function checks for a cached inverse, in which case it returns that value--
## otherwise is computes the inverse and returns it.

## Creates list of function to get/set value of input, get/set inverse of the matrix input.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Checks for cached inverse and returns it if found--otherwise calculates and returns inverse.

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
