## These couple of functions take advantage of R's lexical scoping to cache a matrix and its
## inverse inside an R object and calculate the matrix's inverse only when it hasn't already been
## calculated.

## Function that creates a "special matrix", which in reality is a list containing functions for:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix
## This functions uses the "<<-" operator to set the value of the matrix and of its inverse
## not in the enviornmnet of the functions where the assignation is made but in the environment of the makeCacheMatrix function.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that calculates the inverse of the matrix created with the makeCacheMatrix function.
## However if the inverse of the matrix has already been calculated it returns the value of the cache and
## skips the calculation. Otherwise, it calculates the inverse of the matrix and uses setinverse to
## assign the value of the inverse matrix to the "special matrix".
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
