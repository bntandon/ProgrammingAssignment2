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


## Comments describing this function
## Function 1st gets lower environment variable value for 'm'(<<)
## x is the handle for the list created by the makeCacheMatrix - special vector
## that would have initialized 'm' (<<) to null
## x$getreverse retrieve the exisitng value. In the first call this value is NULL
## if the value is NULL ,message is skipped and 
## x$get is executed which returns the agrument supplied to makeCacheMatrix
## m <- solve(data, ...) then stores the inverse in local m variable
## and x$setreverse(m) passed new value to function(solve) m <<- solve in makeCacheMatrix
## that stores value in 'm' (<<) in the lower environment
## When this function is clled second time, m <- x$getreverse () get non NULL value
## hence message is first printed and returns stored 'm' (<<)  
 
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
