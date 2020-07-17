##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##setwd("~/Jobs Folder/NAACCORD/JHU Course/Coursera/R_Programming/ProgrammingAssignment2")

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
  x <<- y
  k <<- NULL
  }
## `x` (the matrix) and `inverse` (inverse of the matrix).
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## Calculate the inverse of `x`. If the inverse is NULL (the cache has been cleared)
## the inverse is calculated using the `solve()` function. Otherwise the cached value is used to save the computational time.
  k <- x$getInverse()
  if(!is.null(k)){
  message("getting cached data")
  return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}
