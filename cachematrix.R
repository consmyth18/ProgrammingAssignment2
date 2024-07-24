## These functions take a matrix and find its inverse 
## and then store that inverse matrix for later use.
## Basically, starting with a new matrix, you can use cacheSolve  
## to find the inverse and then store that inverse within makeCacheMatrix.
## If you use cacheSolve again on the same matrix, cacheSolve will
## simply find the stored inverse in makeCacheMatrix rather than
## re-running the whole inversing operation.

## This function stores the values of a matrix, specifically
## the matrix itself (in makeCacheMatrix$get()) and its inverse
## (in makeCacheMatrix#getinverse()). It also allows you to set
## the values of a matrix (through makeCacheMatrix$set()). And
## finally it allows you to set the inverse of a matrix
## (through makeCacheMatrix$setinverse()). 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() {
    inv
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function allows you to find the inverse of a matrix.
## If this function has not already been used to find the inverse,
## it finds the inverse matrix and stores it in makeCacheMatrix.
## If the inverse matrix has already been stored, then cacheSolve simply
## retrieves it from makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinverse(inv)
  inv
}
