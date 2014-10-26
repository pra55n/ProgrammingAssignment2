## The following two functions provide a mechanism to cache the inverse of a matrix. 
## The caching trades memory for computation time and can provide significant reduction 
## in run time if the inverse of a matrix is calculated repeatedly.

## makeCacheMatrix function returns a list of functions which form a closure around 
## the supplied matrix x and its inverse. The intention is that these functions are
## passed to cacheSolve instead of directly passing the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  

}


## cacheSolve takes a cached version of matrix created using the makeCacheMatrix 
## function. It uses the function getinv functions in the cached matrix to first 
## check if inverse is already cached, if so returns the cached inverse thus  
## reducing the computation time. Else it computes the inverse using the builtin 
## solve function and caches the inverse using the setinv function in the cached
## function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  inv
}
