## These functions will calculate the inverse of matrix, and cache that value. 
## On subsequent calls to the cached value it will not have to be re-caculated, 
## but can simply be pulled from memory to save time. 

## This function defines the objects x (our matrix) and m (its inverse), and creates
## a list of their values that values that can be called

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
 
   set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This functions reviews the object getinverse to check if a value has been cached.
## If a value is cached, it is returned; if no value is cached (i.e., m is null)
## then the inverse of the matrix supplied to the prior function is evaluated.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
