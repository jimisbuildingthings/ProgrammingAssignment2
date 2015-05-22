#This program can be used to generate the inverse of a (square) matrix and cache the result.  The goal is to avoid performing time consuming computations more than once.

#makeCacheMatrix will take a matrix as an argument (empty by default) and get/set the values of a matrix and its inverse (using the solve function).
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#the cacheSolve function will check if the inverse of the matrix has been set with getinverse.  If it has, the cached result will be returned with a, "getting cached data" message.  If it hasn't the inverse will be calculated by calling setinverse.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}