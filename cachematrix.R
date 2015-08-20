#########################################################################
### Assign2.R
### 08/2015  
### R programming assignment 2 - Using scoping rules to take advantage
### of cache.
############################################################################
emptyTheWorkspace<- function() {
      rm(list = ls())
}
############################################################################
createMatrix <- function() {
      mtxBasic <-  matrix(c(2, 4, 6, 8),  nrow=2,  ncol=2) 
      return(mtxBasic)
}
##############################################################################################
##  This function creates a special "matrix" object 
##  that can cache its inverse.
##############################################################################################
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(mean) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
##############################################################################################
##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
##############################################################################################
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      message("calculating inverse now")
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
