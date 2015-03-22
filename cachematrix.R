## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a list containing a function to: 
  #1.  set the value of the matrix
  #2.  get the value of the matrix
  #3.  set the value of the inverse
  #4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrix_inv <<- solve
  getinverse <- function() matrix_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This function computes the inverse of the special matrix returned by the makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  matrix_inv <- x$getinverse()
  if(!is.null(matrix_inv)) {
    message("getting cached data.")
    return(matrix_inv)
  }
  data <- x$get()
  matrix_inv <- solve(data, ...)
  x$setinverse(matrix_inv)
  matrix_inv
}
