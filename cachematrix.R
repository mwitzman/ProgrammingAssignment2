## cachematrix.R contains two functions. cacheSolve(makeCacheMatrix(matrix)) 
## checks to see if a cached matrix inversion exists, returns the cache if it does, 
## and calculates the inversion if needed (caching it before returning it)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## set the cache location (current matrix)
    x <<- y
    m <<- NULL
  }
  get <- function() x  ## retrieve cache location
  setinverse <- function(solve) m <<- solve    ## set the inverse matrix value in cache
  getinverse <- function() m   ## get the cached inverset matrix value
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Retrieve a value from the cache
    m <- x$getinverse()
    ## message(m)
    ## Check to see if the return is NULL
    if(!is.null(m)) {
      ## if not NULL, return the value from cache
      message("getting cache inverse")
      ## Return the cached inverse matrix
      return(m)
    }
    ## need a check to see if the matrix is unchanged?
    
    ## If no value in cache then solve, cache and return
    inverse <- x$get()
    m <- solve(inverse, ...)
    x$setinverse(m)
    m
  }