## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function makeCacheMatrix create a special "matrix" which has the list containg 
##  1) set the vlaue of the matrix
##  2) get the value of the matirx
##  3) set the value of the solve(inverse)
##  4) get the value of the solve
makeCacheMatrix <- function(x = matrix()) {
        
          m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }

          get <- function() x
          setsolve <- function(solve) m <<- solve
          getsolve <- function() m

          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)

}


## Write a short comment describing this function
## cacheSolve function calculates the inverse of the special "matrix" created with the above function.  However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getsolve()

          if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }

          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
        
}
