## Pair of functions that cache the inverse of a matrix
## Pass the result of a makeCacheMatrix call to cacheSolve 
## x an invertible matrix
## x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
## x$set(matrix(rnorm(16), 4, 4))
makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cmpute and cache the inverse of the matrix
##x the result of a previous makeCacheMatrix call
##additional arguments to pass to solve function
##x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
##cacheSolve(x)
cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
