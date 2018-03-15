## Put comments here that give an overall description of what your
## functions do

## The fcunction below is to create a cache matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_result <- NULL
  set <- function(y) {
    x <<- y
    inverse_result <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_result <<- inverse
  getinverse <- function() inverse_result
  list(set = set,  get = get,
       setinverse = setinverse,  getinverse = getinverse)
}

## The function below is to calculate the inverse of matrix created above, 
## if exist, will use cache value, otherwise, willl use solve function to calculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_result <- x$getinverse()
  if (!is.null(inverse_result)) {
    message("getting cached inverse result")
    return(inverse_result)
  }
  
  message("cached inverse result did not exist, calculating")
  dataMatrix <- x$get()
  inverse_result <- solve(dataMatrix, ...)
  x$setinverse(inverse_result)
  inverse_result
}
