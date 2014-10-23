## These two functions are a framework to manipulate objects of matrix type so
## that the evaluation of the inverse matrix is cached.
##




## a <- makeCacheMatrix(x = matrix())
## This function creates a list of methods to manipulate the cache
## RETURN: a list of functions.
##         - get() returns the matrix (if one)
##         - set(y) load the matrix y into the cache
##         - setinv(inv) save the inverse matrix into the cache
##         - getinv() returns the inverse matrix (if one)
## WARNING: For the function to make sense, x should be a non singular matrix.
makeCacheMatrix <- function(x = matrix()) {
     I <- NULL
     set <- function(y) {
          x <<- y
          I <<- NULL
     }
     get <- function() x
     setinv <- function(inv) I <<- inv
     getinv <- function() I
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}



## a <- cacheSolve(x, ...)
## RETURN: a matrix that is the inverse of the matrix held by the object x.
## WARNING: the matrix x$get() must be non singular.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     I <- x$getinv()
     if(!is.null(I)) {
          message("getting cached data")
          return(I)
     }
     data <- x$get()
     I <- solve(data, ...)
     x$setinv(I)
     I
}
