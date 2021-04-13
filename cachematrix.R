## Program Assignment 2

## Set of functions to cache the inverse of a matrix

## We will use to create a special object that stores a square matrix and caches 
## its inverse.  We will pass the result of makeCacheMatrix call to another function cacheSolve

## Write a short comment describing this function
##
## makeCacheMatrix creates a special “matrix”, which is a list containing a function to:
##   set the elements of the matrix
##   get the elements of the matrix
##   set the elements of the matrix inverse
##   get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                   ## Initializing inverse matrix as NULL Matrix      
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x             ##  function to get a matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##
## Following function calculates the inverse of the special “matrix” created with 
## the above function. It first checks if an inverse has already been calculated. 
## If yes, it gets the inverse from  cache and skips the computation. Else, it calculates 
## inverse of the matrix and sets it in the cache through the set inverse function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {              ## checking whether the inverse is NULL
    message("getting cached matrix inverse")
    return(inv)                    ## Returns the inverse matrix from cache 
  }
  data <- x$get()
  inv <- solve(data, ...)          ## calculates the inverse matrix elements
  x$setinverse(inv)
  inv                              ## Returns a matrix which is inverse of matrix x
}




