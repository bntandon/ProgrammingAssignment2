## Put comments here that give an overall description of what your
## functions do

##  Comments about this function and how to execute it
## Function takes a matrix as its input argument
## for discussion let us say 
## set a test square invertible matrix,
## x <-matrix ( c(-1,-1, 2, 1), 2,2)
## Expected output
## matrix ( c(1,1, -2, -1), 2,2)
## Call the following to make special vector
##  k <- makeCacheMatrix(x)
## This returns a list object with 4 functions
## and initializes 'm'  as NULL as the special object
##  that will store inverse
## Now call cacheSolve first time to calculate Invertible matrix
## cacheSolve(k)
## this returns output. Then call second time
## cacheSolve(k) 
## Second time it prints 'getting cahce data' before  result matrix

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object
  # that can cache its inverse.
  m <- NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <- function () x
  ##browser ("Before setreverse")
  setreverse <- function(solve) m <<- solve
  getreverse <- function () m
  list(set =set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # This function computes the inverse of the special "matrix"
  ## returned by makeCacheMatrix
  m <- x$getreverse ()
  if ( !is.null(m)) {
    message("getting cache data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setreverse(m)
  m  
}
