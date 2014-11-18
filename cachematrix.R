## cachematrix.R contains two functions. makeCacheMatrix maintains
## the cache while cacheSolve checks the cache first and adds
## any new matrix inversions to the cache.

## makeCacheMatrix takes in a matrix, can check to see if 
## a matrix inversion is already in the cache and can add
## new matrix inversions to the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes in a matrix, checks the cache for an inversion,
## and returns the inversion from cache if it exists. 
## If there is no cache value, cacheSolve calculates the inversion, 
## caches it and returns it.

cacheSolve <- function(x, ...) {
    ## Get the inverse value from cache
    m <- x$getinverse()
    ## Check to see if the return is NULL - if not,
    ## return the inverse matrix from cache
    if(!is.null(m)) {
      message("getting cache inverse")
      ## Return a matrix that is the inverse of 'x'
      return(m)
    }
    ## If no value in cache then solve, cache and return
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }