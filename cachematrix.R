## Programming with R - Assignment 2
## This file defines two functions - one to create the matrix and other to 
## cache inverse of a matrix

## This function creates a special matrix which is a list containing
## a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get teh value fo the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates inverse of the special 
## matrix created with above function. However, it first checks
## to see if the inverse has already been calculated. If so, it gets the 
## the inverse from the cache and skips the computation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if (!is.null(m)) {
          message("getting cached data")
          return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
