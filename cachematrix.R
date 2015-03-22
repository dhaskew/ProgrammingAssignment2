## makeCacheMatrix - returns a fancy object and some matrix access funcitons
## which give you access to the matrix and the cache

## cacheSolve - ## Return a matrix that is the inverse of its arg, caching the
## result as necessary



## This function returns a special matrix object that can tell you
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # ensure the cache is empty
  inverse <- NULL
  #setup the set/get/setinverse/getinverse functions
  set <- function(nval){
    x <<- nval
    inverse <- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  #implicit return of fancy object with function references
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## solve() funciton can be used to inverse a square matrix
## http://www.statmethods.net/advstats/matrix.html
## this function will return the cache value, if available, otherwise
## it will solve for the inverse 
cacheSolve <- function(x, ...) {
  # check to see if there is a value in cache already
  inverse <- x$getinverse()
  # if cache is not null, return cached version
  if(!is.null(inverse)) {
    message("getting the data from the cache.")
    return(inverse) # explicit return, since we have a cached value
  }
  # nothing in cache, so we get the data
  data <- x$get()
  # calculate the inverse
  inverse <- solve(data)
  # set the cache for next time
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