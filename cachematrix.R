## This file contains two function for the purpose of caching the inverse of
## a matrix and retreiving the cache of the inverse of a matrix.


## The makeCacheMatrix makes a list which contains the original matrix,
## a location for the inverse of the matrix, and fuctions for retrieving the values 
## of the original matrix, setting the value of the inverse matrix, retrieving the
## value of the inverse matrix, and a fuction both defines the matrix based on the original 
## but also can be recalled for the new variable to redefine the original matrix, which
## resets the value of r the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  setmain <- function(t) {
    x <<- t
    inver <<- NULL
  }
  getmain <- function() x
  setInverse <- function(n) inver <<- n
  getInverse <- function() inver
  list(setmain = setmain, getmain = getmain,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The cacheSolve function takes a list of the form produced by makeCacheMatrix and checks
## to see if the inverse of the matrix has been calculated. If it has been, it will print 
## a message stating that it is retrieving the cached value, and returns the inverse matrix.
## It it has not, it will calculate the inverse matrix and then return inverse matrix.

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$getmain()
  inver <- solve(data, ...)
  x$setInverse(inver)
  inver
  
      ## Return a matrix that is the inverse of 'x'
}
