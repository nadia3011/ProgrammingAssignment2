## This programm is able to cache calculating the inverse of a square -matrix, 
## which is normally a time-consuming computation. When the inverse was already
## computed, the value can be looked up in the cache and is not calculated again

## the function makeCacheMatrix takes a square-matrix as input. it creates a
## "special" matrix, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialize cached inverse with NULL
  inverse <- NULL
  ## set value of matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get value of matrix
  get <- function() x
  ## set value of inverse
  setinverse <- function(solve) inverse <<- solve
  ## get value of inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the "special" matrix created with the
## above function. It first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Other-
## wise, it calculates the inverse of the data and sets the value of the mean in
## the cache via the setinverse()-function.

cacheSolve <- function(x, ...) {

  inverse <- x$getinverse()
  
  ## Check if inverse is saved in the cache. If yes, return inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## if not, calculate inverse
  data <- x$get()
  inverse <- solve(data)
  ## inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  ## output inverse
  inverse
}
