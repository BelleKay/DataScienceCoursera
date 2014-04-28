
## This function creates a special "matrix" object that can cache its inverse

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
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then this function will retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
  store <- x$getsolve()
  if(!is.null(store)) {
    message("getting cached data")
    return(store)
  }make
  data <- x$get()
  store <- solve(data, ...)
  x$setsolve(store) 
  store
}
