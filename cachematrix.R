## The following functions cache the inverse of a matrix in order
## to optimize potentially time-consuming computations.

## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## Function to set value of matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  ## Function to get value of matrix
  get <- function() x
  
  ## Function to set value of inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## Function to get value of inverse 
  getinverse <- function() i
  
  ## List compiling all functions created
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has 
## already been calculated, then cacheSolve retrieves inverse from
## cache

cacheSolve <- function(x, ...) {
  
  ## Defining 'i' with the inverse gotten from makeCacheMatrix
  i <- x$getinverse()
  
  ## Checking if a value for '2' has already been defined
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Getting data (matrix) from makeCacheMatrix
  data <- x$get()
  
  ## Calculating inverse matrix and re-defining 'i'
  i <- solve(data,...)
  x$setinverse(i)
  i
}
