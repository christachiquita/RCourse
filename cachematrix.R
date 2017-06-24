## Christabella C. Bastian, 14/06/2017
## Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
## @input: invertible matrix
## @output: list containing function to set matrix, get matrix, set inverse matrix, get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrix1){
    x <<- matrix1
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## If the matrix has been changed (using set function in makeCacheMatrix), the inv will be set to NULL. 
## Therefore, if we then call the cacheSolve, it will recalculate the inverse instead of using the cached inverse matrix
## @input: list returned by makeCacheMatrix function
## @output: inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)){
    message("getting cached inverse matrix")
    return (inverse)
  }
  inverse <- solve(x$get(), ...)
  x$setInverse(inverse)
  inverse
}
