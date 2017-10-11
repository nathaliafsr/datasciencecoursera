## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function makeMatrix creates a matrix
## It first sets the value of the matrix and then it gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse
  getinverse <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverse()
  if(!is.null(invMatrix)) {
    message("getting cached inversible matrix")
    return(invMatrix)
  }
  Matrixdata <- x$getMatrix()
  invMatrix <- solve(Matrixdata)
  x$setinverse(invMatrix)
  return(invMatrix)
  
  ## cacheSolve returns a matrix that is the inverse of 'x' but 
  ##first checks to see if the mean has already been calculated. 
  ##If so, it gets the mean from the cache and skips the computation. 
  ##Otherwise, it calculates the mean of the data 
  ##and sets the value of the mean in the cache via the setmean function.
}
myMatrix <- matrix(1:4,2,2)

CacheMatrix <- makeCacheMatrix(myMatrix)
CacheMatrix <- makeCacheMatrix(myMatrix)
CacheMatrix$getMatrix()


CacheMatrix$getinverse()

cacheSolve(CacheMatrix)