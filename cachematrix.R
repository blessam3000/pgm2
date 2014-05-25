## This script computes the inverse of a matrix, and saves in a cache. Every consecutive
## checks the cache prior to returning the inverse.
## function makeCachematrix creates the cache
## function cacheSolve checks the cache and if data is not present it inverts it. 
## Asusmption: the input matrix is a square matrix (and invertible)

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL				    # if an object is called without a method
   
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setinv <- function(inv) m <<- inv   # setinv will save the cache
     getinv <- function() m              # getinv will retrieve the cache
     list(set = set, get = get,          # this returns a list of the functions
          setinv = setinv,
          getinv = getinv)
}

cacheSolve <- function(x, ...) {
     m <- x$getinv()                     # checks to see if in cache
     if(!is.null(m)) {                   # if there is data in cache, retrieve it
         message("Inverse available in cache, retrieving it")
         return(m)                       
     }
     data <- x$get()                     # get the data
     m <- solve(data, ...)               # compute the inverse
     x$setinv(m)                         # save in cache
     m
}
