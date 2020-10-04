## This function creates a special "matrix" object that can cache 
## its inverse.

## if X is a square invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y               ##<<- different environments
    matrix_inv <<- NULL
  }
  
  get <- function() x
  set_inv <- function(solve) matrix_inv <<- solve  ## solve calculates the inverse
  get_inv <- function() matrix_inv                 ## get the inverse
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed),then cacheSolve should retrieve the 
## inverse from the cache.

## most of this code is the same as the example in coursera

## we want to return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  matrix_inv <- x$get_inv()         ## we want the inverse
  if(!is.null(matrix_inv)) {        ## if it's not NULL, then it was already calculated
    message("getting cached data")  ## if it's not NULL, it will show the cached data
    return(matrix_inv)
  }
  data <- x$get()                   ## this was based on coursera's example 
  matrix_inv <- solve(data, ...)
  x$set_inv(matrix_inv)
  matrix_inv                        ## print the inverse matrix
}

## To test, we need non singular and square matrix
## How to make a matrix: "matrix(1:9, nrow = 3, ncol = 3)"
## here is one example of a valid matrix called example_2
## example_2 <-makeCacheMatrix(cbind(c(6,5),c(2,3)))


