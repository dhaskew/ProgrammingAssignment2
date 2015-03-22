## Put comments here that give an overall description of what your
## functions do

## This function returns a special matrix object that can tell you
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(nval){
    x <<- nval
    inverse <- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Return a matrix that is the inverse of 'x'
## solve() funciton can be used to inverse a square matrix
## http://www.statmethods.net/advstats/matrix.html
## this function will return the cache value, if available, otherwise
## it will solve for the inverse 
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting the data from the cache.")
    return(inverse) # explicit return, since we have a cached value
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse #implicit return
}

# create a 2x2 matrix for testing
mat = matrix(1:4,nr=2)
# get the fancy object with cache/function references
fancy_object = makeCacheMatrix(mat)
# show the fancy object
fancy_object
# show the original matrix
fancy_object$get()
# first run - should have no print message
cacheSolve(fancy_object)
# second run - should have print message from cache retrieval
cacheSolve(fancy_object)