## The idea here is to develop two functions that calculate the inverse of a matrix
## but cache its values to be used more times without recalculating

## Create the structure necessary for the caching mechanism of the matrix inverse
makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,setinverse= setinverse,getinverse = getinverse)
}

## This function calculates the inverse of the matrix 'x' using the solve function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}


