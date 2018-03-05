## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      M <- NULL
      set <- function(y) {
            x <<- y
            M <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) M <<- solve
      
      getinverse <- function() M
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      M <- x$getinverse()
      if(!is.null(M)) {
            message("getting cached data")
            return(M)
      }
      data <- x$get()
      M <- solve(data, ...)
      x$setinverse(M)
      M
}
