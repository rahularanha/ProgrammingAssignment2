## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix - defines setters and getters for a matrix and its inverse
## x - object for input matrix ; im - object for inverse matrix of 'x'

makeCacheMatrix <- function(x = matrix()) {
     
     im <- NULL
     set <- function(y) {
          x <<- y
          im <<- NULL
     }
     get <- function() x
     setinv <- function(inv) im <<- inv
     getinv <- function() im
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)

}


## cacheSolve - if inverse of matrix is not calculated (null), it
##              calculates the inverse and returns it, otherwise
##              it returns the old (cached) inverse matrix value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     im <- x$getinv()
     if(!is.null(im)) {
          message("getting cached data")
          return(im)
     }
     data <- x$get()
     im <- solve(data, ...)
     x$setinv(im)
     im
}
