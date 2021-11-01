## These functions will calculate the inverse of matrix, and cache that value. 
## On subsequent calls to the cached value it will not have to be re-caculated, 
## but can simply be pulled from memory to save time. 

## This function defines the objects x (our matrix) and m (its inverse), and creates
## a list of their values that values that can be called

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
 
   set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This functions reviews the object getinverse to check if a value has been cached.
## If a value is cached, it is returned; if no value is cached (i.e., m is null)
## then the inverse of the matrix supplied to the prior function is evaluated.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
