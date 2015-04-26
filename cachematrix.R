## Calculating the inverse of a matrix can be a costly operation. This
## file caches the inverse of a matrix so that it only needs to be 
## calculated once. To use this file, input a matrix into makeCacheMatrix
## and save the output. Then run cacheSolve on this new variable. This
## will return the inverse of the matrix and it will also cache it for
## later use.

## This function takes in a numeric matrix (which we assume is invertable)
## and it creates four functions for it: set, get, setinverse, and 
## getinverse. It will allow us to cache the inverse of the matrix for
## future work.

makeCacheMatrix <- function(x = matrix()) {
  ## i will store the inverse of the matrix
  i <- NULL
  
  ## Set the matrix and clear the old inverse since we don't know it yet
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Return the matrix
  get <- function() x
  
  ## Save the inverse for later retrieval
  setinverse <- function(inverse) i <<- inverse
  
  ## Return the inverse of the matrix
  getinverse <- function() i
  
  ## Return the functions that are associated with this matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## Check if we already have the inverse of the matrix and return
  ## it if we do
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Since we didn't have the inverse, we'll have to calculate it
  ## and then save it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}