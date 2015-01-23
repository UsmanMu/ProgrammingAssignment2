## Matrix inversion is usually a costly computation 
## If the matrix has been cached before, the inverse of a matrix
## will be time saving ratherthan compute it repeatedly 

## The following two functions used to find the inverse of a matrix

## makeCacheMatrix  
## cacheSolve

## The first function, makeCacheMatrix creates a special matrix object 
## that can cache its inverse, is a list of functions that can
 ## set the value of value of matrix
 ## get the value of matrix
 ## set the inverse of the matrix
 ## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

## The next function, cacheSolve return the inverse of the above matrix 'x'
## created with the makeCacheMatrix function. If the cached inverse has 
## been computed and available, cacheSolve retrieves it and skips the computation. 
## If not, the inverse of matrix is computed, saved to caches, and returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

