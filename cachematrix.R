## Program that caches the solution of the inverse of a matrix. 
## After the function has been called once, the solution is stored
## and need not be recomputed during subsequent function calls.

## makeCacheMatrix takes a matrix as an argument and returns a list of functions.
## the <<- operatorin setinverse means that this assignment statement alters the 
## value of inv inside the getinverse function.

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve takes list of functions returned by makeCacheMatrix as its argument.
## If cacheSolve has not been called before for that particular argument, it computes
## the inverse of the matrix, and this value is stored in makeCacheMatrix via setinverse
## This stored value is simply retrieved during any subsequent calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
