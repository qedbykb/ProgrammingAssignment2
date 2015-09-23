## R Programming assignment 2: The first of the two functions 
## below will take a non-singular matrix 'm' and cache its
## inverse. The second function will return the inverse of 
## a matrix, provided the function is passed a "cached matrix"
## like the one returned by the first function.


## The 'makeCacheMatrix' function takes a matrix 'm' as an 
## argument and returns a list of four functions: set, get, 
## setinv, and getinv. Calling makeCacheMatrix(m)$get() will
## return the matrix 'm', and calling 
## makeCacheMatrix(m)$getinv() will return the inverse of 'm'.

makeCacheMatrix <- function(m) {
   minv <- solve(m)
   set <- function(a) {
      m <<- a
      minv <<- solve(m)
   }
   get <- function() m
   setinv <- function(inverse) minv <<- inverse
   getinv <- function() minv
   list(set = set, get = get, setinv = setinv, 
        getinv = getinv)
}


## The 'cacheSolve' function takes a cached matrix 'x' as an
## argument. The cached matrix 'x' is a list containing 
## functions whose arguments are either empty, or a numeric
## matrix 'm'. 'x' must contain the function getinv(), which
## returns the inverse of 'm', and the function get(), which 
## returns 'm'. The output of 'cacheSolve' is the inverse of 
## the matrix 'm', which is computed either by finding it in 
## the cache of 'x' or by computing it on the spot if its not
## in the cache of 'x'. 

cacheSolve <- function(x) {
   s <- x$getinv()
   if(!is.null(s)) {
      message("retrieving cached data")
      return(s)
   }
   newmat <- x$get()
   newmatinv <- solve(newmat)
   newmatinv
}
